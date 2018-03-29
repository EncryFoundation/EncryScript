package encrywm.frontend.semantics

import org.scalatest.{Matchers, PropSpec}
import utils.SourceProcessor

class OptimizerTest extends PropSpec with Matchers with SourceProcessor {

  property("Simple AST optimization") {

    val tree = precess(
      """
        |let longName = 999
        |let anotherLongName: String = "SomeString"
      """.stripMargin)

    val optimized = Optimizer.scan(tree)
  }
}
