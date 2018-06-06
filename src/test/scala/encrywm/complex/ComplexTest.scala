package encrywm.complex

import encrywm.common.SourceValidator
import encrywm.lang.backend.executor.Execution
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

object Utils {
  implicit class Traceable[T](val obj: T) extends AnyVal {
    def trace: T = { println(obj); obj}
    def traceWith[S](reader: T => S ): T = { println(reader(obj)); obj}
  }
}
import encrywm.common.SourceProcessor._

object ScriptGenerator {
  trait Expression {
    def render: String = this match {
      case Padding(padding) => Seq.fill(padding)("  ").mkString
      case Block(padding, expr) => expr.map(padding.render + _.render).mkString("\n")
      case BracesBlock(expr@_*) => s"{${expr.map(_.render).mkString("\n")}}"
      case Let(name, value) => s"let $name = ${value.render}"
      case Str(value) => s""""$value""""
      case Base58(value) => s"""base58"$value""""
      case Raw(value) => value
      case TypedArg(name, tp) => s"$name: $tp"
      case FuncDefinition(name, rt, args, body) =>
        s"def $name( ${args.map(_.render).mkString(", ")}) -> $rt: \n${body.map(_.render).mkString("\n")}"
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
  case class Block(padding: Padding, expr: Seq[Expression]) extends Expression
  case class Let(name: Name, value: Expression) extends Expression
  case class Str(value: String) extends Expression
  case class Raw(value: String) extends Expression
  case class Padding(value: Int = 0) extends Expression
  def block(expr: Expression*): Block = Block(Padding(0), expr)
}

object ScriptSamples {
  import ScriptGenerator._
  def base58const: Base58 = Base58("11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox")
  def lets(n: Int): String = (0 to n).map(i => Let(s"x$i", base58const).render).mkString("\n")
  val funcInvokeExample: String = Func.invoke("foo",
    Func.invoke("bar", "10", Base58("asddd")),
    Func.invoke("baz", "15")
  ).render

  val funcDefineExample: String = Func.define("foo", "Unit","a" -> "Int", "b" -> "String")(
    Let("x", Base58("11")),
    Let("x", Base58("11"))
  ).render

  val example1: String = block(
    Func.define("f", "Int", "a" -> "Int")("return (a + 1)"),
    Func.define("g", "Unit", "a" -> "Int")("unlock if f(a) == 1"),
//    Func.invoke("f", "0")
    Func.invoke("f", "0")
  ).render

  val example2: String = block(
    Func.define("f", "Int", "a" -> "Int")("  return (a + 1)"),
    //    Func.invoke("f", "0")
    Func.invoke("f", "0")
  ).render

  val example3: String = block(
    Func.define("f", "Unit", "a" -> "Int")("  unlock if a + 1 == 1", "  return"),
    //    Func.invoke("f", "0")
    Func.invoke("f", "0")
  ).render

  val loooongInt: String = block(
    Let("a", Seq.fill(100)("1").mkString).render
  ).render

  val loooongString: String = block(
    Let("a", Str(Seq.fill(10000)("a").mkString)).render,
    "a"
  ).render

  def samplesFromFile(path: String, separator: String = "\n\n\n"): Seq[String] =
    Try(scala.io.Source.fromFile(path).mkString.split(separator).toSeq).getOrElse(Seq.empty)
}

class ComplexTest extends PropSpec with Matchers with Execution {
  import ScriptSamples._

  property("samples validation") {
    samplesFromFile("test/samples.esc")
      .++(Seq(example1, example2, example3, loooongString, base58const.render, funcDefineExample, funcInvokeExample))
      .map(SourceValidator.validateSource)
      //.traceWith(_.mkString("\n"))
      .count(_.isRight) shouldEqual 5
  }

  property("execute valid samples") {
    samplesFromFile("test/samples.esc")
      .++(Seq(example1, example2, example3, loooongString))
      .filter(SourceValidator.validateSource(_).isRight)
      .map(process)
      .map(_.map(exc.executeContract))
      .forall(_.isSuccess) shouldBe true
  }
}
