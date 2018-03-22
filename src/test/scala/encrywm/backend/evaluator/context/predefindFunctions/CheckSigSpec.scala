package encrywm.backend.evaluator.context.predefindFunctions

import encrywm.backend.evaluator.context.ESValue
import encrywm.builtins.Types.{BYTE_VECTOR, INT}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random

class CheckSigSpec extends PropSpec with Matchers {

  property("testEval") {

    val args = Seq(
      ESValue("signature", BYTE_VECTOR)(Random.randomBytes()),
      ESValue("message", BYTE_VECTOR)(Random.randomBytes()),
      ESValue("public key", BYTE_VECTOR)(Random.randomBytes())
    )

    val funcRes = CheckSig().eval(args)

    funcRes.isSuccess shouldBe true
  }

  property("Incorrect args count") {

    val args = Seq(
      ESValue("arg1", BYTE_VECTOR)(Random.randomBytes()),
      ESValue("arg2", BYTE_VECTOR)(Random.randomBytes()),
      ESValue("arg3", BYTE_VECTOR)(Random.randomBytes()),
      ESValue("arg4", BYTE_VECTOR)(Random.randomBytes())
    )

    val funcRes = CheckSig().eval(args)

    funcRes.isFailure shouldBe true
  }

  property("Incorrect args type"){

    val privkey = Curve25519.createKeyPair(Random.randomBytes())

    val randomText = Random.randomBytes()

    val signature = Curve25519.sign(privkey._1, randomText)

    val args = Seq(
      ESValue("signature", BYTE_VECTOR)(signature),
      ESValue("message", INT)(1),
      ESValue("public key", BYTE_VECTOR)(privkey._2)
    )

    val funcRes = CheckSig().eval(args)

    funcRes.isFailure shouldBe true
  }

  property("Correct signature check"){

    val privkey = Curve25519.createKeyPair(Random.randomBytes())

    val randomText = Random.randomBytes()

    val signature = Curve25519.sign(privkey._1, randomText)

    val args = Seq(
      ESValue("signature", BYTE_VECTOR)(signature),
      ESValue("message", BYTE_VECTOR)(randomText),
      ESValue("public key", BYTE_VECTOR)(privkey._2)
    )

    val funcRes = CheckSig().eval(args)

    funcRes.get.value shouldBe true
  }

  property("Incorrect signature check"){

    val privkey = Curve25519.createKeyPair(Random.randomBytes())

    val signature = Curve25519.sign(privkey._1, Random.randomBytes())

    val args = Seq(
      ESValue("signature", BYTE_VECTOR)(signature),
      ESValue("message", BYTE_VECTOR)(Random.randomBytes()),
      ESValue("public key", BYTE_VECTOR)(privkey._2)
    )

    val funcRes = CheckSig().eval(args)

    funcRes.get.value shouldBe false
  }
}
