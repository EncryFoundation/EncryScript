package encrywm.lang.frontend.semantics

import encrywm.ast.Ast
import encrywm.lang.frontend.parser.Statements
import encrywm.lib.TypeSystem
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class StaticProcessorSpec extends PropSpec with Matchers {

  property("Semantically correct simple AST analysis") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 9
        |let b: Bool = true
        |let c: String = "string"
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Semantically incorrect simple AST analysis (Undefined name)") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: undef = 9
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Valid AST with nested scope analysis") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Valid AST with function call analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
        |
        |sum(1, 2)
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Invalid AST with nested scope analysis (Undefined name)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + c
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Invalid AST with undefined function call analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |sum()
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Analysis of invalid AST with wrong number of args passed to function") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + c
        |
        |sum(1, 2, 3)
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Valid If-expression") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 9 if 6 > 10 else 0
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Invalid If expression and undefined ref inside") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 9 if b < 0 else 0
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Valid lambda def") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |lamb (a: Int, b: Int) = a * b
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Invalid lambda def (unresolved name in body)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |lamb (a: Int, b: Int) = a * c
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Analysis of valid AST with If statement") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |if 2 == 2 and 8 == 10:
        |    let a = 100
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Analysis of invalid AST with If statement and undefined ref in test part") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |if a > 0 and b < 0:
        |    let a = 100
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Valid AST analysis with attribute referencing") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let timestamp = context.transaction.timestamp
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Invalid AST analysis with nonexistent attribute referencing") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let timestamp = transaction.attr0
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Valid lambda application to coll") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let list = [1, 2, 3, 4, 5]
        |
        |list.exists(lamb (n: Int) = n > 10)
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Valid function application to coll") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let list = [1, 2, 3, 4, 5]
        |
        |def isGreaterThan10(n: Int) -> Bool:
        |    return n > 10
        |
        |list.exists(isGreaterThan10)
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("`.map()` application to coll") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let list = [1, 2, 3, 4, 5]
        |
        |let mapped = list.map(lamb (n: Int) = n * 100)
        |
        |mapped[2].get + 10
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Invalid `.map()` application to coll (Wrong number of arguments)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let list = [1, 2, 3, 4, 5]
        |
        |let mapped = list.map(lamb (n: Int, s: String) = n)
        |
        |mapped[2].get + 10
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Inalid lambda application to coll (Wrong argument type)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let list = [1, 2, 3, 4, 5]
        |
        |list.exists(lamb (n: String) = n > 10)
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Invalid function application to coll (Wrong argument type)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let list = [1, 2, 3, 4, 5]
        |
        |def isGreaterThan10(n: String) -> Bool:
        |    return n > 10
        |
        |list.exists(isGreaterThan10)
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  // Type checking

  property("Type checking of valid assignment") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = 9L + 1
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Type checking of invalid assignment (Type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 9L + 1
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Type checking of valid function definition") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Type checking of invalid function definition (Type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> String:
        |    return a + b
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
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

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Type checking of invalid function call (Argument type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
        |
        |sum(1, "string")
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Type checking of invalid assignment (Zero division in value part)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = 1 / 0
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Type checking of valid list") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = [1, 2, 3, 4]
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Type checking of invalid list (Elms types mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = [1, 2, 3L, 4]
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Type checking of invalid list (Nested coll)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = [1, 2, [1, 2, 3], 4]
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Type checking of valid dict") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = {1 : "string", 2 : "string"}
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Type checking of invalid dict (Elts types mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = {1 : "string", 2 : 8}
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Type checking of invalid dict (Nested coll)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a = {1 : {1 : 2}, 2 : {3 : 4}}
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Type checking of valid list subscription") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let lst = [1, 2, 3, 4]
        |let a: Int = lst[1].get
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Type checking of invalid list subscription (Type mismatch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let lst = [1, 2, 3, 4]
        |let a: String = lst[1]
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Valid Base58 string analysis") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let byteVector: Bytes = base58"11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox"
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Invalid Base58 string analysis (Illegal symbol)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let byteVector = base58"invalidString_oO0"
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
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

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Valid match statement with many statements in the branch") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def func(a: Int) -> Int:
        |   match context.proof:
        |       case sig -> Signature25519:
        |           let b = 100
        |           return b * a
        |       case _:
        |           return 0 * 1000
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }

  property("Return-type checking inside match statement branches (Wrong type [String])") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def func(a: Int) -> Int:
        |   match context.proof:
        |       case sig -> Signature25519:
        |           let b = 100
        |           return b * a
        |       case _:
        |           return "string"
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Return-type checking inside match statement branches (Wrong type -> `pass` statement should be treated as `Unit` return-type)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def func(a: Int) -> Int:
        |   match context.proof:
        |       case sig -> Signature25519:
        |           let b = 100
        |           return b * a
        |       case _:
        |           pass
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Invalid match statement (No default branch)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |match context.proof:
        |   case sig -> Signature25519:
        |       sig.sigBytes
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Scoping (Name referenced from outer scope)") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |if true:
        |   let a = 5
        |
        |a
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe false
  }

  property("Scoping with global modifier") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |if true:
        |   global let a = 5
        |
        |a
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    AstRoot.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val processR = sp.process(AstRoot.get.value)

    processR.isSuccess shouldBe true
  }
}
