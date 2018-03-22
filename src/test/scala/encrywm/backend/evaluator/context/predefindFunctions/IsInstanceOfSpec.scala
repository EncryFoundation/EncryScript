package encrywm.backend.evaluator.context.predefindFunctions

import encrywm.backend.evaluator.context.{ESObject, ESValue}
import encrywm.builtins.Types.{FLOAT, INT}
import org.scalatest.{FunSuite, Matchers, PropSpec}

class IsInstanceOfSpec extends PropSpec with Matchers {

  property("Correct type compare") {

    val args = Seq(
      ESValue("arg1", INT)(2),
      ESValue("arg2", INT)(3)
    )

    val result = IsInstanceOf().eval(args)

    result.get.value shouldBe true

  }

  property("Incorrect type compare") {

    val args = Seq(
      ESValue("arg1", INT)(2),
      ESValue("arg2", FLOAT)(3f)
    )

    val result = IsInstanceOf().eval(args)

    result.get.value shouldBe false

  }

}
