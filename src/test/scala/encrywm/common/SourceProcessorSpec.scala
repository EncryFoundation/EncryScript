package encrywm.common

import org.scalatest.{Matchers, PropSpec}

class SourceProcessorSpec extends PropSpec with Matchers {

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
        |schema PersonBox:Object(
        |    name:String;
        |    age:Int;
        |)
        |
        |#---script---
        |
        |def checkAge(box: Box) -> Bool:
        |   match box:
        |       case personBox -> @PersonBox:
        |           return personBox.body.age > 20
        |       case _:
        |           pass
      """.stripMargin

    val procTry = SourceProcessor.process(s)

    procTry.isSuccess shouldBe true
  }

  property("Composite source processing (Unresolved ref in schema)") {
    val s =
      """
        |schema PersonBox:Object(
        |    name:String;
        |    age:Null;
        |)
        |
        |#---script---
        |
        |def checkAge(box: Box) -> Bool:
        |   match box:
        |       case personBox -> @PersonBox:
        |           return personBox.body.age > 20
        |       case _:
        |           pass
      """.stripMargin

    val procTry = SourceProcessor.process(s)

    procTry.isSuccess shouldBe false
  }
}
