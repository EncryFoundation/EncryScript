package encrywm.frontend

import org.scalatest.{Matchers, PropSpec}
import utils.SourceProcessor

class AstOptimizerTest extends PropSpec with Matchers with SourceProcessor {

  property("Simple AST optimization") {

    val tree = precess(
      """
        |let longName = 999
        |let anotherLongName: String = "SomeString"
      """.stripMargin)

    val optimized = AstOptimizer.scan(tree)
  }
}
