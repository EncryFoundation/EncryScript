package encrywm.utils.mast

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.lang.frontend.parser.Statements
import encrywm.lang.frontend.semantics.StaticProcessor
import encrywm.lib.TypeSystem
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
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val processR = sp.process(AstRoot.get.value)

    val separatedContracts = Mast.separateContract(processR.get)

    """
      |let a = 1
      |let b = a + 2
      |unlock if b < 10000
    """.stripMargin.trim shouldEqual separatedContracts.head.toString
  }

  property("Separation of heavy contract by boolean operations in unlockIf") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = 1
        |let b = a + 2
        |let c = 4
        |let d = 8
        |let e = 14
        |unlock if (b < 10000) || ((c + d) > 8 && (e < 20))
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val processR = sp.process(AstRoot.get.value)

    val separatedContracts = Mast.separateContract(processR.get)

    """
      |let a = 1
      |let b = a + 2
      |unlock if b < 10000
    """.stripMargin.trim shouldEqual separatedContracts.head.toString

    """
      |let c = 4
      |let d = 8
      |let e = 14
      |unlock if c + d > 8 && e < 20
    """.stripMargin.trim shouldEqual separatedContracts.last.toString
  }

  property("Separation of heavy contract by ifStmt") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = 1
        |let b = a + 2
        |let c = 4
        |if (c > 2) :
        |   let b = 3
        |else :
        |   let a = 4
        |unlock if (b < 10000) || (c > 2 && a < 2)
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val processR = sp.process(AstRoot.get.value)

    val separatedContracts = Mast.separateContract(processR.get)

    """
      |let c = 4
      |if (c > 2):
      |let b = 3
      |unlock if b < 10000
    """.stripMargin.trim shouldEqual separatedContracts.head.toString

    """
      |let a = 1
      |let c = 4
      |unlock if c > 2 && a < 2
    """.stripMargin.trim shouldEqual separatedContracts(1).toString

    """
      |let a = 1
      |let b = a + 2
      |unlock if b < 10000
    """.stripMargin.trim shouldEqual separatedContracts(2).toString

    """
      |let c = 4
      |if (!(c > 2)):
      |let a = 4
      |unlock if c > 2 && a < 2
    """.stripMargin.trim shouldEqual separatedContracts.last.toString
  }

  property("Separation of heavy contract by match stmts") {

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

    """
      |let b = 1
      |match b:
      |  case 1:
      |unlock if true
      |  case _:
      |unlock if true
    """.stripMargin.trim shouldEqual separatedContracts.head.toString

    """
      |let a = 2
      |match a:
      |  case 1:
      |unlock if true
      |  case _:
      |unlock if true
    """.stripMargin.trim shouldEqual separatedContracts(1).toString

    """
      |let b = 1
      |match b:
      |  case 2:
      |unlock if true
      |  case _:
      |unlock if true
    """.stripMargin.trim shouldEqual separatedContracts(2).toString

    """
      |let a = 2
      |match a:
      |  case 2:
      |unlock if true
      |  case _:
      |unlock if true
    """.stripMargin.trim shouldEqual separatedContracts.last.toString
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
    }.map(contract => Mast.mastRootFromContracts(Seq(contract))).distinct.length shouldBe contractCount
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

    val unlockContract = Utils.getContractHash(processR(unlockContractSource.get.value))

    Mast.mastRootFromHashes(unlockContract +: contracts.tail.map(Utils.getContractHash)) shouldEqual mastRoot
  }
}
