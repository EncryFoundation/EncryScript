package encrywm.builtins

import encrywm.frontend.parser.Ast.TYPE

case class Attribute(name: String, tpe: TYPE, value: Any)

trait ESObject {

  val name: String

  val attrs: Set[Attribute]
}
