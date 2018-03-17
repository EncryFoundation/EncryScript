package encrywm.builtins

import encrywm.frontend.ast.Ast.TYPE

case class Attribute(name: String, tpe: TYPE, value: Any)

case class ESObject(name: String, attrs: Set[Attribute])
