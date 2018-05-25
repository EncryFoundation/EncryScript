package encrywm.utils.mast

import encrywm.ast.Ast
import encrywm.ast.Ast.EXPR.UnaryOp
import encrywm.ast.Ast.STMT.UnlockIf
import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.Ast.UNARY_OP.Not
import encrywm.ast.Ast._
import encrywm.utils.mast.Utils._

object Mast {

  type ContractRootHash = Array[Byte]

  def mastRootFromContracts(contracts: Seq[Contract]): ContractRootHash =
    listRootHash(contracts.map(Utils.getContractHash).sortWith(_.zip(_).forall(bytes => bytes._1 > bytes._2)).toList)

  def mastRootFromHashes(contractsHashes: Seq[Hash]): ContractRootHash =
    listRootHash(contractsHashes.sortWith(_.zip(_).forall(bytes => bytes._1 > bytes._2)).toList)

  def separateContract(contract: Contract): Seq[Contract] =
    simplifyContract(contract).foldLeft(Seq[Contract]()) {
      case (contracts, simpleContract) =>
        contracts ++ simpleContract.body.reverse.foldLeft(Seq[Contract]()) {
          case (contractsFromSimple, simpleContractStmt) =>
            contractsFromSimple ++ getUnlocks(simpleContractStmt).flatMap(unlockIfStmtWithVariables =>
              createContractFromSTMT( simpleContract.body.takeWhile( _ != unlockIfStmtWithVariables._1 ).reverse, unlockIfStmtWithVariables._2, unlockIfStmtWithVariables._1 )
            )
        }
    }.distinct

  /**
    * getUnlocks - find STMT.unlockIf in stmt and return seq of stmts, which contains unlockIfSTMT with variables name which used in UnlockIfSTMT
    * If get UnlockIf(boolOp1 || boolOp2) separate them to UnlockIf(boolOp1), UnlockIf(boolOp2)
    * If get IfStmt witch contains body and orElse part, with unlockIf stmt in each of them, separate if stmt to 2 stmt.
    * First ifStmt will contains only body, second will have revert ifStmt.test by Not, and body = orElse
    */
  private def getUnlocks(stmt: STMT): Seq[(STMT, Seq[VariableName])] = stmt match {
    case unlock: STMT.UnlockIf => splitUnlockIf(unlock).map(unlockStmt => unlockStmt -> unlockStmt.variables)
    case matchSTMT: STMT.Match =>
      matchSTMT.branches.foldLeft(Seq[(STMT, Seq[VariableName])]()) {
        case (unlocksInMatchSTMT, branchSTMT) =>
          unlocksInMatchSTMT ++ getUnlocks(branchSTMT).map(matchBranch => matchSTMT -> (matchBranch._2 ++ matchSTMT.target.variables))
      }
    case ifSTMT: STMT.If =>
      ifSTMT.body.foldLeft(Seq[(STMT, Seq[VariableName])]()) {
        case (bodySeq, bodyStmt) =>
          bodySeq ++
            getUnlocks( bodyStmt ).map( stmtWithUnlock => ifSTMT.copy( orelse = List.empty[Ast.STMT] ) -> (stmtWithUnlock._2 ++ ifSTMT.test.variables))
      } ++ ifSTMT.orelse.foldLeft(Seq[(STMT, Seq[VariableName])]()) {
        case (orElseStmts, orElseStmt) =>
          orElseStmts ++
            getUnlocks(orElseStmt).map(stmtWithUnlock =>
              ifSTMT.copy(test = UnaryOp(UNARY_OP.Not, ifSTMT.test), body = ifSTMT.orelse, orelse = List.empty[Ast.STMT]) -> (stmtWithUnlock._2 ++ ifSTMT.test.variables))
      }
    case caseStmt: STMT.Case =>
      caseStmt.body.foldLeft(Seq[(STMT, Seq[VariableName])]()) {
        case (caseSeq, bodyStmt) =>
          caseSeq ++ getUnlocks(bodyStmt)
      }
    case _ => Seq()
  }

  /** splitUnlockIf - splitUnlockIfStmt by boolOps */
  private def splitUnlockIf(unlockIf: STMT.UnlockIf): Seq[UnlockIf] =
    unlockIf.test match {
      case boolOp: EXPR.BoolOp =>
        val split = splitBoolOps(boolOp)
        split.map(op => UnlockIf(op))
      case _ => Seq(unlockIf)
    }

  /**
    * Split boolOps by ||
    * Examples:
    * ((0 < 3) || (1 < 2)) -> ((0 < 3)), (1 < 2)
    * ((0 < 3) & (1 < 2)) -> ((0 < 3) & (1 < 2))
    * (((0 < 3) & (1 < 2)) || (3 < 10)) -> ((0 < 3) & (1 < 2)), (3 < 10)
    */
  private def splitBoolOps(boolOp: EXPR.BoolOp): Seq[EXPR] =
    boolOp.op match {
      case BOOL_OP.Or => boolOp.values.foldLeft(Seq[EXPR]()) {
        case (seq, expr) => expr match {
          case bool: EXPR.BoolOp => seq ++ splitBoolOps(bool)
          case comp: EXPR.Compare => seq :+ comp
          case _ => seq
        }
      }
      case _ => Seq(boolOp)
    }

  /**
    * Create ast "way" to unlockIf stmt, by removing unused stmt from origin ast.
    * Example:
    * |let a = 2        |
    * |let b = a + 3    |         |let a = 2        |
    * |let c = 4        |  ---->  |let b = a + 3    |
    * |let d = 5        |         |Unlock if b > 2  |
    * |Unlock if b > 2  |
    */
  private def createContractFromSTMT(stmts: Seq[STMT], variables: Seq[VariableName], fromSTMT: STMT): Option[Contract] = {
    val stmtsAfterDrop: (Seq[STMT], Seq[Ast.VariableName]) = dropRedundantSTMTs(stmts, variables)
    if(stmtsAfterDrop._2.isEmpty) Some(Contract((fromSTMT +: stmtsAfterDrop._1.toList).reverse)) else None
  }

  private def dropRedundantSTMT(stmt: STMT, variablesToDrop: Seq[Ast.VariableName]): (STMT, Seq[Ast.VariableName]) = stmt match {
    case let: STMT.Let =>
      if (variablesToDrop.contains(let.variableName)) let -> (variablesToDrop.filter(_ != let.variableName) ++ let.variables)
      else let -> variablesToDrop
    case ifStmt: STMT.If =>
      val resultIfBody: (Seq[STMT], Seq[Ast.VariableName]) = dropRedundantSTMTs(ifStmt.body, variablesToDrop)
      if (resultIfBody._1.nonEmpty) ifStmt.copy(body = resultIfBody._1.toList) -> (resultIfBody._2 ++ ifStmt.test.variables)
      else ifStmt -> variablesToDrop
    case matchStmt: STMT.Match =>
      if (variablesToDrop.contains(matchStmt.target.variables.head)) {
        val mainBranchBody: (STMT, Seq[Ast.VariableName]) = dropRedundantSTMT(matchStmt.branches.head, variablesToDrop)
        val genericCondBody: (STMT, Seq[Ast.VariableName]) = dropRedundantSTMT(matchStmt.branches.last, variablesToDrop)
        val variablesNames: Seq[Ast.VariableName] = if (mainBranchBody._2.length > genericCondBody._2.length) mainBranchBody._2 else genericCondBody._2
        matchStmt.copy(branches = List(mainBranchBody._1, genericCondBody._1)) -> variablesNames
      } else matchStmt -> variablesToDrop
    case caseStmt: STMT.Case =>
      val resultCaseBody: (Seq[STMT], Seq[Ast.VariableName]) = dropRedundantSTMTs(caseStmt.body, variablesToDrop)
      caseStmt.copy(body = resultCaseBody._1.toList) -> resultCaseBody._2
    case uselessStmt => uselessStmt -> variablesToDrop
  }

  private def dropRedundantSTMTs(stmts: Seq[STMT], variables: Seq[Ast.VariableName]): (Seq[STMT], Seq[Ast.VariableName]) =
    stmts.foldLeft(Seq[STMT](), variables) {
      case (resultIf, bodyStmt) =>
        val dropStmt: (STMT, Seq[Ast.VariableName]) = dropRedundantSTMT(bodyStmt, resultIf._2)
        if (dropStmt._2 != resultIf._2) (resultIf._1 :+ bodyStmt) -> dropStmt._2 else resultIf
    }


  /**
    * simplifyContract - separate heavy contract to simple by ifSTMT and matchSTMT, but not optimize them
    * (Simple contract - contract, which contains only 2 branches in each matchStmt, only body in ifStmt and only one condition in unlockIf)
    * Example:
    * |let a = 2          |
    * |let b = 1          |
    * |                   |         |let a = 2          |         |let a = 2          |
    * |match b:           |         |let b = 1          |         |let b = 1          |
    * |case 1:            |   -->   |match b:           |   and   |match b:           |
    * |   unlock if true  |         |case 1:            |         |case 2:            |
    * |case 2:            |         |    unlock if true |         |    unlock if true |
    * |   unlock if true  |         |case _:            |         |case _:            |
    * |case _:            |         |    unlock if true |         |    unlock if true |
    * |   unlock if true  |
    */
  private def simplifyContract(contract: Contract): Seq[Contract] = {
    contract.body.tail.foldLeft(Seq[Contract](Contract(List(contract.body.head)))) {
      case (contracts, stmt) => stmt match {
        case matchStmt: STMT.Match =>
          contracts.foldLeft(Seq[Contract]()) {
            case (newContracts, prevContract) =>
              newContracts ++ matchStmt.branches.dropRight(1).foldLeft(Seq[Contract]()) {
                case (contractByBranches, branch) =>
                  contractByBranches :+ prevContract.copy(body = prevContract.body :+ STMT.Match(matchStmt.target, List(branch, matchStmt.branches.last)))
              }
          }
        case ifStmt: STMT.If =>
          contracts.foldLeft(Seq[Contract]()) {
            case (newContracts, prevContract) =>
              val newContractsWihtIfBody: Seq[Contract] = newContracts :+ prevContract.copy(body = prevContract.body :+ ifStmt.copy(orelse = List.empty))
              if (ifStmt.orelse.nonEmpty)
                newContractsWihtIfBody :+ prevContract.copy(body = prevContract.body :+ ifStmt.copy(test = UnaryOp(Not, ifStmt.test), body = ifStmt.orelse, orelse = List.empty))
              else newContractsWihtIfBody
          }
        case simStmt =>
          contracts.map(prevContract => prevContract.copy(body = prevContract.body :+ simStmt))
      }
    }
  }
}
