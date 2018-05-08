package encrywm.frontend.semantics

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.backend.executor.Execution
import encrywm.common.SourceProcessor
import org.scalatest.{Matchers, PropSpec}

class OptimizerTest extends PropSpec with Matchers with Execution {

  property("Simple AST optimization") {

    def sample(i: Int) = s"let longName$i = 999\n"

    val tree = SourceProcessor.process((0 to 100).map(sample).reduce(_ + _))

    val optimized = Optimizer.scan(tree.get)

    val execR = exc.executeContract(optimized.asInstanceOf[Contract])

    execR.isRight shouldBe true
  }
}
