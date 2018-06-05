package encrywm.complex

import encrywm.common.SourceValidator
import org.scalatest.{Matchers, PropSpec}

object Utils {
  implicit class Traceable[T](val obj: T) extends AnyVal {
    def trace = {println(obj);obj}
    def traceWith[S](reader: T => S ) = {println(reader(obj)); obj}
  }
}
import Utils._

object CodeSource {
  val example1= """
    |// Constant definition
    |let a: Int = 10                     // Explicit type declaration
    |let b = 100                         // Type will be inferred automatically
    |let c = true if a < b else false    // Conditional assignment
  """.stripMargin
}

class ComplexTest extends PropSpec with Matchers {
  property("*") {
    val validated = SourceValidator.validateSource(CodeSource.example1)
    validated.isRight shouldBe true
  }

}
