package frontend.semantics

import encrywm.ast.Ast
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.StaticAnalyser
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class StaticAnalyserSpec extends PropSpec with Matchers {

  property("Semantically correct simple AST analysis") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: int = 9
        |let b: bool = true
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Semantically incorrect simple AST analysis (Undefined name)") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: undef = 9
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST with nested scope analysis") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + b
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Valid AST with function call analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + b
        |
        |sum(1, 2)
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Invalid AST with nested scope analysis (Undefined name)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + c
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Invalid AST with undefined function call analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |sum()
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Analysis of invalid AST with wrong number of args passed to function") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + c
        |
        |sum(1, 2, 3)
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST with If-expression analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: int = 9 if 6 > 10 else 0
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Analysis of invalid AST with If expression and undefined ref inside") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: int = 9 if b < 0 else 0
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Analysis of valid AST with If statement") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |if 2 == 2 and 8 == 10:
        |    let a = 100
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Analysis of invalid AST with If statement and undefined ref in test part") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |if a > 0 and b < 0:
        |    let a = 100
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST analysis with attribute referencing") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let timestamp = transaction.timestamp
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Invalid AST analysis with nonexistent attribute referencing") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let timestamp = transaction.attr0
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  // Type checking

  property("Type checking of valid assignment") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = 9L + 1
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Type checking of invalid assignment (Type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: int = 9L + 1
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of valid function definition") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + b
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Type checking of invalid function definition (Type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> string:
        |    return a + b
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of invalid function definition (Return type mismatch) (Nested statements scanning)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    if a > b:
        |        return a
        |    else:
        |        return 0L
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of invalid function call (Argument type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + b
        |
        |sum(1, "string")
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of invalid assignment (Zero division in value part)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: int = 1 / 0
      """.stripMargin)

    val analyzer = StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }
}
