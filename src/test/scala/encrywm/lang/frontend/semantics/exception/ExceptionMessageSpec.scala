package encrywm.lang.frontend.semantics.exception

import encrywm.ast.Ast
import encrywm.lang.frontend.parser.Statements
import encrywm.lang.frontend.semantics.StaticProcessor
import encrywm.lib.TypeSystem
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Failure

class ExceptionMessageSpec extends PropSpec with Matchers {

  property("Incorrect type") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let a: Int = "test"
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val processR = sp.process(AstRoot.get.value)

    processR match {
      case Failure(e) => e.getMessage shouldEqual "Expected type Int, got String. In 'let a: Int = \"test\"'"
    }
  }

  property("Incorrect return type") {

    val AstRoot = (Statements.contract ~ End).parse(
      """
        |def sum(a: String) -> Int:
        |    return a
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val processR = sp.process(AstRoot.get.value)

    processR match {
      case Failure(e) => e.getMessage shouldEqual "Expected type Int, got String. In 'def sum(a: String): -> Int:\nreturn a'"
    }
  }

  property("Incorrect list") {
    val AstRoot = (Statements.contract ~ End).parse(
      """
        |let list = [1, 2, "str", 4, 5]
      """.stripMargin)

    val sp = new StaticProcessor(TypeSystem.default)

    val processR = sp.process(AstRoot.get.value)

    processR match {
      case Failure(e) => e.getMessage shouldEqual "Expected type Int, got String. In '[ 12\"str\"45 ]'"
    }
  }

}
