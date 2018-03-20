package encrywm.builtins

import encrywm.builtins.Types.TYPE

case class Attribute(name: String, tpe: TYPE)

case class ComplexType(name: String, attrs: Seq[Attribute])
