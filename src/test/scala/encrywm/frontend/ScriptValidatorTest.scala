package encrywm.frontend

import encrywm.common.ScriptValidator
import org.scalatest.{Matchers, PropSpec}

class ScriptValidatorTest extends PropSpec with Matchers {

  property("Simple contract validation from file") {
    val filePath = "test/contract_valid_1.esc"
    val validated = ScriptValidator.validateFromFile(filePath)

    validated.isRight shouldBe true
  }

  property("Invalid contract validation from file") {
    val filePath = "test/contract_invalid_1.esc"
    val validated = ScriptValidator.validateFromFile(filePath)

    validated.isRight shouldBe false
  }
}
