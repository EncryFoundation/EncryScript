package utils

/* Created on 07.06.18 */
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

