package encrywm.frontend.semantics

case class NameError(n: String) extends Error(s"Name $n is undefined.")

case object IllegalExprError extends Error(s"Unexpected expression.")

case object IllegalOperandError extends Error(s"Illegal operand type.")

case object ZeroDivisionError extends Error(s"Zero division.")

case object MissedContextError extends Error(s"Missed context for AST processing.")

case class WrongNumberOfArgumentsError(fn: String) extends Error(s"Wrong number of arguments in $fn.")

case class NotAnObjectError(n: String) extends Error(s"$n is not an object.")

case class TypeMismatchError(t1: String, t2: String) extends Error(s"Expected type is $t1, got $t2.")
