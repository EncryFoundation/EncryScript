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
  def base58const = """base58"11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox""""
  trait Expression {
    def render: String
  }
  case class BracesBlock(expr: Expression) extends Expression {
    override def render: String = s"{${expr.render}}"
  }
  case class Let(name: String, value: String) extends Expression {
    override def render: String = s"""let $name = $value"""
  }
  def lets(n: Int) = (0 to n).map(i => Let(s"x$i", base58const).render).mkString("\n")
}

import CodeSource._

class ComplexTest extends PropSpec with Matchers {
  property("*") {
    val codeSource = lets(100)
    //val codeSource = lets(1000) //при этом значении тест падает со StackOverflowException
    val validated = SourceValidator.validateSource(codeSource)
    validated.isRight shouldBe true
  }

}
