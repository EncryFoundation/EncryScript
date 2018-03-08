import fastparse.core.Parsed
import org.scalatest.{Matchers, PropSpec}

class CalculatorTest extends PropSpec with Matchers {

  import encrywm.Calculator._

  property("01") {

    val Parsed.Success(res1, _) = expr.parse("1+4")
    val Parsed.Success(res2, _) = expr.parse("1+(4*9)")
    val Parsed.Success(res3, _) = expr.parse("1*(6/2)")
    val Parsed.Success(res4, _) = expr.parse("7*4*9*9")

    res1 shouldEqual 5
    res2 shouldEqual 37
    res3 shouldEqual 3
    res4 shouldEqual 2268
  }
}
