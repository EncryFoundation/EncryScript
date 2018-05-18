package encrywm.utils.mast

import encrywm.ast.Ast.STMT.UnlockIf
import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.Ast._
import encrywm.utils.mast.Utils._

object Mast {

  type ContractRootHash = Array[Byte]

  def mastRoot(contracts: Seq[Contract]): ContractRootHash = listRootHash(contracts.map(_.hash).toList)

  def separateContracts(contract: Contract): Seq[Contract] =
    contract.body.foldLeft(Map[String, (Seq[String], Seq[STMT])]()){
      case (map, stmt) =>
        val variables = stmt.variables
        stmt match {
          case unlock: STMT.UnlockIf =>
            map ++ splitUnlockIf(unlock).foldLeft(Map[String, (Seq[String], Seq[STMT])]()){
              case (unlockMap, unlockStmt) => unlockMap +
                    (map.size + unlockMap.size.toString ->
                      (Seq.empty[String], createNodeLine(map, unlockStmt.variables) :+ unlockStmt))
            }
          case _ =>
            map + (variables.last -> (variables.dropRight(1), Seq(stmt)))
        }
    }.filter(_._1.forall(_.isDigit))
      .map(unlockInfo => Contract(unlockInfo._2._2.toList)).toSeq


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
    * @param boolOp
    * @return
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

  //TODO: Rename
  private def createNodeLine(varsMap: Map[String, (Seq[String], Seq[STMT])], stmtVars: Seq[String]): Seq[STMT] =
    stmtVars.foldLeft(Seq[STMT]()) {
      case (globalSeq, varName) =>
        globalSeq ++ varsMap.get(varName).map(varInfo => {
          varInfo._1.foldLeft(Seq[STMT]()) {
            case (seq, refVarName) =>
              seq ++ varsMap.get(refVarName).map(refVarInfo => createNodeLine(varsMap - refVarName, refVarInfo._1)).getOrElse(Seq.empty[STMT])
          } ++ varInfo._2
        }
        ).getOrElse(Seq.empty[STMT])
    }
}
