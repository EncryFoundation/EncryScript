package encrywm.frontend

import encrywm.common.SourceValidator
import org.scalatest.{Matchers, PropSpec}

class SourceValidatorTest extends PropSpec with Matchers {

  property("Simple contract validation from file") {
    val filePath = "test/contract_valid_1.esc"
    val validated = SourceValidator.validateFromFile(filePath)

    validated.isRight shouldBe true
  }

  property("Invalid contract validation from file") {
    val filePath = "test/contract_invalid_1.esc"
    val validated = SourceValidator.validateFromFile(filePath)

    validated.isRight shouldBe false
  }
}
