package frontend.serializer

import org.scalatest.{Matchers, PropSpec}
import encrywm.frontend.ast.AstCodec._

class SerializerTest extends PropSpec with Matchers {

  property("Contract serializer"){

    val encoded = codec.encode(Generator.generateContract)

    encoded.isSuccessful shouldBe true
  }
}
