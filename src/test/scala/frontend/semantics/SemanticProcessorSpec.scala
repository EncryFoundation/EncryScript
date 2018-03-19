package frontend.semantics

import encrywm.frontend.ast.Ast
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.SemanticProcessor
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class SemanticProcessorSpec extends PropSpec with Matchers {

  property("Type checking of valid assignment") {

    val treeParsed = (Statements.contract ~ End).parse(
      """
        |let a = 9L + 1
      """.stripMargin)

    treeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val treeRoot = treeParsed.get.value

    val processTry = SemanticProcessor.processTree(treeRoot)

    processTry.get

    processTry.isSuccess shouldBe true
  }

  property("Type checking of invalid assignment (Type mismatch)") {

    val treeParsed = (Statements.contract ~ End).parse(
      """
        |let a: int = 9L + 1
      """.stripMargin)

    treeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val treeRoot = treeParsed.get.value

    val processTry = SemanticProcessor.processTree(treeRoot)

    processTry.isSuccess shouldBe false
  }

  property("Type checking of valid function definition") {

    val treeParsed = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + b
      """.stripMargin)

    treeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val treeRoot = treeParsed.get.value

    val processTry = SemanticProcessor.processTree(treeRoot)

    processTry.isSuccess shouldBe true
  }

  property("Type checking of invalid function definition (Type mismatch)") {

    val treeParsed = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> string:
        |    return a + b
      """.stripMargin)

    treeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val treeRoot = treeParsed.get.value

    val processTry = SemanticProcessor.processTree(treeRoot)

    processTry.isSuccess shouldBe false
  }

  property("Type checking of invalid function definition (Return type mismatch) (Nested statements scanning)") {

    val treeParsed = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    if a > b:
        |        return a
        |    else:
        |        return 0L
      """.stripMargin)

    treeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val treeRoot = treeParsed.get.value

    val processTry = SemanticProcessor.processTree(treeRoot)

    processTry.isSuccess shouldBe false
  }

  property("Type checking of invalid function definition (Argument type mismatch)") {

    val treeParsed = (Statements.contract ~ End).parse(
      """
        |def sum(a: int, b: int) -> int:
        |    return a + b
        |
        |sum(1, "string")
      """.stripMargin)

    treeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val treeRoot = treeParsed.get.value

    val processTry = SemanticProcessor.processTree(treeRoot)

    processTry.isSuccess shouldBe false
  }

  property("Type checking of invalid assignment (Type mismatch) (If-expression scanning)") {

    val treeParsed = (Statements.contract ~ End).parse(
      """
        |let a: int = 1 if 0 == 0 else "string"
      """.stripMargin)

    treeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val treeRoot = treeParsed.get.value

    val processTry = SemanticProcessor.processTree(treeRoot)

    processTry.isSuccess shouldBe false
  }

  property("Type checking of invalid assignment (Zero division in value part)") {

    val treeParsed = (Statements.contract ~ End).parse(
      """
        |let a: int = 1 / 0
      """.stripMargin)

    treeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val treeRoot = treeParsed.get.value

    val processTry = SemanticProcessor.processTree(treeRoot)

    processTry.isSuccess shouldBe false
  }
}
