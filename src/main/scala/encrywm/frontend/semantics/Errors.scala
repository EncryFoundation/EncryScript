package encrywm.frontend.semantics

case class NameError(n: String) extends Error(s"Name $n is undefined.")

case class IllegalExprError(e: String) extends Error(s"Unexpected expression in $e.")

case class WrongNumberOfArgumentsError(fn: String) extends Error(s"Wrong number of arguments in $fn.")

case class NotAnObjectError(n: String) extends Error(s"$n is not an object.")
