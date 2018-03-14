package encrywm.builtins

import encrywm.parser.Ast.TYPE

case class Attribute(name: String, tpe: TYPE, value: Any)

case class ESObject(name: String, attrs: Map[String, Attribute])
