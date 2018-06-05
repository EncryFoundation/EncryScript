package encrywm.complex

import encrywm.common.SourceValidator
import org.scalatest.{Matchers, PropSpec}

object Utils {
  implicit class Traceable[T](val obj: T) extends AnyVal {
    def trace: T = { println(obj); obj}
    def traceWith[S](reader: T => S ): T = { println(reader(obj)); obj}
  }
}
import Utils._

object ScriptGenerator {
  trait Expression {
    def render: String = this match {
      case BracesBlock(expr@_*) => s"{${expr.map(_.render).mkString("\n")}}"
      case Let(name, value) => s"let $name = ${value.render}"
      case Str(value) => s""""$value""""
      case Base58(value) => s"""base58"$value""""
      case Raw(value) => value
      case TypedArg(name, tp) => s"$name: $tp"
      case FuncDefinition(name, rt, args, body) =>
        s"def $name( ${args.map(_.render).mkString(", ")}) -> $rt: \n${body.map(_.render).mkString("\n")} \n"
      case FuncInvoke(name, args) => s"$name(${args.map(_.render).mkString(", ")})"
    }
  }

  type Type = String
  type Name = String
  implicit def str2expr: String => Raw = Raw(_)
  implicit def pair2TypedArg(x: (String, String)): TypedArg = TypedArg(x._1, x._2)
  object Func {
    def define(name: Name, returnType: Type, args: TypedArg*)(body: Expression*) = FuncDefinition(name, returnType, args, body)
    def invoke(name: Name, args: Expression*) = FuncInvoke(name, args)
  }
  case class TypedArg(name: Name, tp: Type) extends Expression
  case class FuncDefinition(name: Name, returnType: Type, args: Seq[TypedArg], body: Seq[Expression]) extends Expression
  case class FuncInvoke(name: Name, args: Seq[Expression]) extends Expression
  case class Base58(value: String) extends Expression
  case class BracesBlock(expr: Expression*) extends Expression
  case class Let(name: Name, value: Expression) extends Expression
  case class Str(value: String) extends Expression
  case class Raw(value: String) extends Expression
}

object ScriptSamples {
  import ScriptGenerator._
  def base58const = Base58("11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox")
  def lets(n: Int): String = (0 to n).map(i => Let(s"x$i", base58const).render).mkString("\n")
  val funcInvokeExample = Func.invoke("foo",
    Func.invoke("bar", "10", Base58("asddd")),
    Func.invoke("baz", "15")
  ).render

  val funcDefineExample = Func.define("foo", "Unit","a" -> "Int", "b" -> "String")(
    Let("x", Base58("11")),
    Let("x", Base58("11"))
  ).render
}

class ComplexTest extends PropSpec with Matchers {
  import ScriptSamples._
  property("*") {
    val validated1 = SourceValidator.validateSource(lets(100))
    val validated2 = SourceValidator.validateSource(funcDefineExample)
    val validated3 = SourceValidator.validateSource(funcInvokeExample)
  }
}
