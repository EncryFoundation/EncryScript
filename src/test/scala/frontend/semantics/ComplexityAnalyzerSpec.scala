package frontend.semantics

import encrywm.ast.Ast
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.{ComplexityAnalyzer, StaticAnalyser}
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class ComplexityAnalyzerSpec extends PropSpec with Matchers {

  property("testScan of 2 stmt") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: int = 9
        |let b: bool = true
      """.stripMargin)

    val analyzer = ComplexityAnalyzer

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true

    analyzeTry.get shouldEqual 2
  }

  property("testScan of simple if stmt ") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: int = 9 if b < 0 else 0
      """.stripMargin)

    val analyzer = ComplexityAnalyzer

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true

    analyzeTry.get shouldEqual 4

  }

  property("testScan of list with 4 elem") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = [1, 2, 3, 4]
      """.stripMargin)

    val analyzer = ComplexityAnalyzer

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true

    analyzeTry.get shouldEqual 4
  }

  property("testScan of function with call") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + c
        |
        |sum(1, 2, 3)
      """.stripMargin)

    val analyzer = ComplexityAnalyzer

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true

    analyzeTry.get shouldEqual 7
  }

}

