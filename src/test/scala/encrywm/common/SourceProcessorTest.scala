package encrywm.common

import org.scalatest.{Matchers, PropSpec}

class SourceProcessorTest extends PropSpec with Matchers {

  property("Script source processing") {
    val s =
      """
        |let a = 10
      """.stripMargin

    val procTry = SourceProcessor.process(s)

    procTry.isSuccess shouldBe true
  }

  property("Schema source processing") {
    val s =
      """
        |schema Person:Object(
        |    name:String;
        |    age:Int;
        |)
      """.stripMargin

    val procTry = SourceProcessor.process(s)

    procTry.isSuccess shouldBe false
  }

  property("Composite source processing") {
    val s =
      """
        |schema Person:Object(
        |    name:String;
        |    age:Int;
        |)
        |
        |#---script---
        |
        |let a = 10
      """.stripMargin

    val procTry = SourceProcessor.process(s)

    procTry.isSuccess shouldBe true
  }
}
