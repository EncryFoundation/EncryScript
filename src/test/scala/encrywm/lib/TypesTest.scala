package encrywm.lib

import encrywm.lib.Types._
import org.scalatest.{Matchers, PropSpec}

class TypesTest extends PropSpec with Matchers {

  property("Product subtype checking with isSubtypeOf()`") {

    Signature25519.isSubtypeOf(ESProof) shouldBe true
  }

  property("Getting attr type of supertype with getAttrType()") {

    Signature25519.getAttrType("typeId") shouldBe Some(ESInt)
  }
}
