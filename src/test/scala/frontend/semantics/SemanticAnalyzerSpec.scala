package frontend.semantics

import encrywm.frontend.parser.Ast.{EXPR, OPERATOR}
import encrywm.frontend.parser.{Ast, Statements}
import encrywm.frontend.semantics.SemanticAnalyzer
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class SemanticAnalyzerSpec extends PropSpec with Matchers {

  property("Semantically correct simple AST analysis") {

    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |let a: int = 9
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Semantically incorrect simple AST analysis (Undefined name)") {

    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |let a: undef = 9
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST with nested scope analysis") {

    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + b
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Valid AST with function call analysis") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + b
        |
        |sum(1, 2)
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Invalid AST with nested scope analysis (Undefined name)") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + c
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Invalid AST with undefined function call analysis") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |sum()
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Analysis of invalid AST with wrong number of args passed to function") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + c
        |
        |sum(1, 2, 3)
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST with If-expression analysis") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |let a: int = 9 if 6 > 10 else 0
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Analysis of invalid AST with If expression and undefined ref inside") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |let a: int = 9 if b < 0 else 0
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Analysis of valid AST with If statement") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |if 2 == 2 and 8 == 10:
        |    let a = 100
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Analysis of invalid AST with If statement and undefined ref in test part") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |if a > 0 and b < 0:
        |    let a = 100
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST analysis with attribute referencing") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |let timestamp = transaction.timestamp
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Invalid AST analysis with nonexistent attribute referencing") {
    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |let timestamp = transaction.attr0
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type calculation (INT is expected)") {

    val analyzer = new SemanticAnalyzer

    val tpe = analyzer.getType(EXPR.BinOp(EXPR.IntConst(9), OPERATOR.Add, EXPR.IntConst(9)))

    tpe shouldBe Ast.TYPE.INT
  }

  property("Type calculation (LONG is expected)") {

    val analyzer = new SemanticAnalyzer

    val tpe = analyzer.getType(EXPR.BinOp(EXPR.IntConst(9), OPERATOR.Mult, EXPR.LongConst(9)))

    tpe shouldBe Ast.TYPE.LONG
  }
}
