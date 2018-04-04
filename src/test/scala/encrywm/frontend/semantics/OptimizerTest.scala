package encrywm.frontend.semantics

import encrywm.ast.Ast.TREE_ROOT
import encrywm.backend.executor.Execution
import org.scalatest.{Matchers, PropSpec}
import utils.SourceProcessor

class OptimizerTest extends PropSpec with Matchers with SourceProcessor with Execution {

  property("Simple AST optimization") {

    def sample(i: Int) = s"let longName$i = 999\n"

    val tree = precess((0 to 100).map(sample).reduce(_ + _))

    val optimized = Optimizer.scan(tree).asInstanceOf[TREE_ROOT.Contract]

    val execR = exc.executeContract(optimized)

    execR.isRight shouldBe true
  }
}
