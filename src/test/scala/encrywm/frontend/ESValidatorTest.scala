package encrywm.frontend

import org.scalatest.{Matchers, PropSpec}

class ESValidatorTest extends PropSpec with Matchers {

  property("Simple contract validation from file") {
    val filePath = "/Users/oskin/Desktop/Dev/scala_dev/EncryScript/test/contract_valid_1.esc"
    val validated = ESValidator.validateFromFile(filePath)

    validated.isRight shouldBe true
  }

  property("Invalid contract validation from file") {
    val filePath = "/Users/oskin/Desktop/Dev/scala_dev/EncryScript/test/contract_invalid_1.esc"
    val validated = ESValidator.validateFromFile(filePath)

    validated.isRight shouldBe false
  }
}