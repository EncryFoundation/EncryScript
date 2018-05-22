package encrywm.utils.mast

import encrywm.ast.Ast.COMP_OP.Lt
import encrywm.ast.Ast.EXPR._
import encrywm.ast.Ast.EXPR_CTX.{Load, Store}
import encrywm.ast.Ast.Identifier
import encrywm.ast.Ast.OPERATOR.Add
import encrywm.ast.Ast.STMT.{Case, Let, Match, UnlockIf}
import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.lang.frontend.parser.Statements
import encrywm.lang.frontend.semantics.StaticProcessor
import encrywm.lib.TypeSystem
import encrywm.lib.Types.ESInt
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Random

class MastTest extends PropSpec with Matchers {

  property("Optimization of simple contract") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = 1
        |let b = a + 2
        |let c = 4
        |unlock if b < 10000
        |let d = 2
        |unlock if d < 10000
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val processR = sp.process(AstRoot.get.value)

    val separatedContracts = Mast.separateContract(processR.get)

    separatedContracts.head shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("d"), Store, None), None), IntConst(2)),
          UnlockIf(Compare(Name(Identifier("d"), Load, Some(ESInt)), List(Lt), List(IntConst(10000)))
          )
        )
      )

    separatedContracts.last shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("a"), Store, None) ,None), IntConst(1)),
          Let(Declaration(Name(Identifier("b"), Store, None), None), BinOp(Name(Identifier("a"), Load, Some(ESInt)), Add, IntConst(2), Some(ESInt))),
          UnlockIf(Compare(Name(Identifier("b"), Load, Some(ESInt)), List(Lt), List(IntConst(10000))))
        )
      )
  }

  property("Separation of heavy contract") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = 2
        |let b = 1
        |
        |match a:
        |    case 1:
        |        unlock if true
        |    case 2:
        |        unlock if true
        |    case _:
        |        unlock if true
        |
        |match b:
        |    case 1:
        |        unlock if true
        |    case 2:
        |        unlock if true
        |    case _:
        |        unlock if true
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val processR = sp.process(AstRoot.get.value)


    //Separate contract to 4 contracts
    val separatedContracts = Mast.separateContract(processR.get)

    separatedContracts.head shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("b"), Store, None) , None), IntConst(1)),
          Match(
            Name(Identifier("b"), Load, Some(ESInt)),
            List(
              Case(IntConst(1), List(UnlockIf(True))),
              Case(GenericCond, List(UnlockIf(True)), isDefault = true)
            )
          )
        )
      )

    separatedContracts(1) shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("a"), Store, None) , None), IntConst(2)),
          Match(Name(Identifier("a"),Load,Some(ESInt)),
            List(
              Case(IntConst(1), List(UnlockIf(True))),
              Case(GenericCond, List(UnlockIf(True)), isDefault = true)
            )
          )
        )
      )

    separatedContracts(2) shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("b"), Store, None), None), IntConst(1)),
          Match(
            Name(Identifier("b"), Load, Some(ESInt)),
            List(
              Case(IntConst(2), List(UnlockIf(True))),
              Case(GenericCond, List(UnlockIf(True)), isDefault = true)
            )
          )
        )
      )

    separatedContracts.last shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("a"), Store, None), None), IntConst(2)),
          Match(
            Name(Identifier("a"), Load, Some(ESInt)),
            List(
              Case(IntConst(2), List(UnlockIf(True))),
              Case(GenericCond, List(UnlockIf(True)), isDefault = true)
            )
          )
        )
      )
  }

  property("Different contracts should give different hashes") {

    def contract(i: Int) =
      (Statements.contract ~ End).parse(
      s"""
        |let a: Int = $i
        |unlock if (a > 11)
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val contractCount: Int = Random.nextInt(100)

    (0 until contractCount).foldLeft(Seq[Contract]()){
      case (contractsSeq, i) => contractsSeq :+ sp.process(contract(i).get.value).get
    }.map(_.hash).distinct.length shouldBe contractCount
  }

  property("Check correct mast Root") {

    val mainContract = (Statements.contract ~ End).parse(
      """
        |let a = 2
        |let b = 1
        |
        |match a:
        |    case 1:
        |        unlock if true
        |    case 2:
        |        unlock if true
        |    case _:
        |        unlock if true
        |
        |match b:
        |    case 1:
        |        unlock if true
        |    case 2:
        |        unlock if true
        |    case _:
        |        unlock if true
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    def processR(contract: Contract) = sp.process(contract).get

    val contracts = Mast.separateContract(processR(mainContract.get.value))

    val mastRoot = Mast.mastRootFromContracts(contracts)

    //1 contract
    val unlockContractSource =
      (Statements.contract ~ End).parse(
        s"""
           |let b = 1
           |
           |match b:
           |  case 1:
           |    unlock if true
           |  case _:
           |    unlock if true
      """.stripMargin)

    val unlockContract = processR(unlockContractSource.get.value).hash

    Mast.mastRootFromHashes(unlockContract +: contracts.tail.map(_.hash)) shouldEqual mastRoot
  }
}
