package encrywm.complex

import encrywm.common.SourceValidator
import encrywm.lang.ESCompiler
import encrywm.lang.backend.executor.Execution
import org.scalatest.{Matchers, PropSpec}

class ComplexTest extends PropSpec with Matchers with Execution {
  import ScriptSamples._

  property("samples validation") {
    samplesFromFile("test/samples.esc")
      .++(Seq(example1, example2, example3, loooongString, base58const.render, funcDefineExample, funcInvokeExample))
      .map(SourceValidator.validateSource)
      //.traceWith(_.mkString("\n"))
      .count(_.isRight) shouldEqual 5
  }

  property("execute valid samples") {
    samplesFromFile("test/samples.esc")
      .++(Seq(example1, example2, example3, loooongString))
      .filter(SourceValidator.validateSource(_).isRight)
      .map(ESCompiler.compile)
      .map(_.map(exc.executeContract))
      .forall(_.isSuccess) shouldBe true
  }
}
