package encrywm.backend.evaluator

import encrywm.ast.Ast.AST_NODE
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.StaticAnalyser
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class ExecutorSpec extends PropSpec with Matchers {

  def precess(s: String): AST_NODE = {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    StaticAnalyser.scan(parsed)
    parsed
  }

  property("Evaluation test") {

    val tree = precess(
      """
        |let a = 999
        |let b = 9
        |a + b
        |unlock
      """.stripMargin)

    val exc = new Executor

    val excR = exc.execute(tree)

    println(tree)

    println(excR)
  }
}
