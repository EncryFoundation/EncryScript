package encrywm.builtins

import encrywm.builtins.Types.TYPE


case class Attribute(name: String, tpe: TYPE, value: Any)

case class ESObject(name: String, attrs: Seq[Attribute])
