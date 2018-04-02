package encrywm.frontend.semantics

import encrywm.ast.Ast
import encrywm.frontend.parser.Statements
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class StaticAnalyserSpec extends PropSpec with Matchers {

  property("Semantically correct simple AST analysis") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 9
        |let b: Bool = true
        |let c: String = "string"
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Semantically incorrect simple AST analysis (Undefined name)") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: undef = 9
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST with nested scope analysis") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Valid AST with function call analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
        |
        |sum(1, 2)
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Invalid AST with nested scope analysis (Undefined name)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + c
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Invalid AST with undefined function call analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |sum()
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Analysis of invalid AST with wrong number of args passed to function") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + c
        |
        |sum(1, 2, 3)
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST with If-expression analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 9 if 6 > 10 else 0
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Analysis of invalid AST with If expression and undefined ref inside") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 9 if b < 0 else 0
      """.stripMargin)

    val analyzer = new StaticAnalyser

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

    val analyzer = new StaticAnalyser

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

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid AST analysis with attribute referencing") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let timestamp = context.transaction.timestamp
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Invalid AST analysis with nonexistent attribute referencing") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let timestamp = transaction.attr0
      """.stripMargin)

    val analyzer = new StaticAnalyser

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

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Type checking of invalid assignment (Type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 9L + 1
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of valid function definition") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Type checking of invalid function definition (Type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> String:
        |    return a + b
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of invalid function definition (Return type mismatch) (Nested statements scanning)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    if a > b:
        |        return a
        |    else:
        |        return 0L
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of invalid function call (Argument type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
        |
        |sum(1, "string")
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of invalid assignment (Zero division in value part)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 1 / 0
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of valid list") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = [1, 2, 3, 4]
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Type checking of invalid list (Elms types mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = [1, 2, 3L, 4]
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of invalid list (Nested coll)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = [1, 2, [1, 2, 3], 4]
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of valid dict") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = {1 : "string", 2 : "string"}
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Type checking of invalid dict (Elms types mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = {1 : "string", 2 : 8}
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of invalid dict (Nested coll)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = {1 : {1 : 2}, 2 : {3 : 4}}
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Type checking of valid list subscription") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let lst = [1, 2, 3, 4]
        |let a: Int = lst[1]
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Type checking of invalid list subscription (Type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let lst = [1, 2, 3, 4]
        |let a: String = lst[1]
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid Base58 string analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let byteVector: Bytes = base58"11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox"
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Invalid Base58 string analysis (Illegal symbol)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let byteVector = base58"invalidString_oO0"
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Valid match statement") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |match context.proof:
        |   case sig -> Signature25519:
        |       sig.sigBytes
        |   case _:
        |       0 * 1000
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Invalid match statement (No default branch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |match context.proof:
        |   case sig -> Signature25519:
        |       sig.sigBytes
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Scoping (Name referenced from outer scope)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |if true:
        |   let a = 5
        |
        |a
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe false
  }

  property("Scoping with global modifier") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |if true:
        |   global let a = 5
        |
        |a
      """.stripMargin)

    val analyzer = new StaticAnalyser

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.scan(AstRoot.get.value))

    analyzeTry.isSuccess shouldBe true
  }
}
