package frontend.serializer

import org.scalatest.{Matchers, PropSpec}
import encrywm.ast.AstCodec._

class AstCodecSpec extends PropSpec with Matchers {

  property("Contract serializer"){

    val encoded = codec.encode(Generator.generateContract)

    encoded.isSuccessful shouldBe true
  }
}
