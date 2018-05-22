package encrywm.utils.mast

import encrywm.ast.Ast.EXPR._
import encrywm.ast.Ast.EXPR_CTX.{Load, Store}
import encrywm.ast.Ast.Identifier
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

  property("Separation of simple contract with function"){
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


    //Separate contract to 2 contracts
    val separatedContracts = Mast.separateContract(processR.get)

    separatedContracts.head shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("b"),Store,None),None),IntConst(1),false),
          Match(
            Name(Identifier("b"),Load,Some(ESInt)),
            List(
              Case(IntConst(1),List(UnlockIf(True)),false),
              Case(GenericCond,List(UnlockIf(True)),true)
            )
          )
        )
      )

    separatedContracts(1) shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("a"),Store,None),None),IntConst(2),false),
          Match(Name(Identifier("a"),Load,Some(ESInt)),
            List(
              Case(IntConst(1),List(UnlockIf(True)),false),
              Case(GenericCond,List(UnlockIf(True)),true)
            )
          )
        )
      )

    separatedContracts(2) shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("b"),Store,None),None),IntConst(1),false),
          Match(
            Name(Identifier("b"),Load,Some(ESInt)),
            List(
              Case(IntConst(2),List(UnlockIf(True)),false),
              Case(GenericCond,List(UnlockIf(True)),true)
            )
          )
        )
      )

    separatedContracts.last shouldEqual
      Contract(
        List(
          Let(Declaration(Name(Identifier("a"),Store,None),None),IntConst(2),false),
          Match(
            Name(Identifier("a"),Load,Some(ESInt)),
            List(
              Case(IntConst(2),List(UnlockIf(True)),false),
              Case(GenericCond,List(UnlockIf(True)),true)
            )
          )
        )
      )
  }

  property("Different contracts should give different hashes"){
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
}
