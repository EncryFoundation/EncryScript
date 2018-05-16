package encrywm.lang.frontend.semantics.exceptions

class SemanticException(m: String) extends Exception(m)

case class NameException(n: String) extends SemanticException(s"Name $n is undefined.")

case class AlreadyDefinedException(n: String) extends SemanticException(s"Name $n is already defined in scope.")

case class UnexpectedStatementException(s: String) extends SemanticException(s"Unexpected statement: $s.")

case object IllegalExprException extends SemanticException(s"Unexpected expression.")

case object IllegalOperandException extends SemanticException(s"Illegal operand type.")

case object ZeroDivisionException extends SemanticException(s"Zero division.")

case object MissedContextException extends SemanticException(s"Missed context for AST processing.")

case class WrongNumberOfArgumentsException(fn: String) extends SemanticException(s"Wrong number of arguments in $fn.")

case class UnapplicableFunctionException(fn: String, coll: String) extends SemanticException(s"$fn is not applicable to $coll")

case class NotAnObjectException(n: String) extends SemanticException(s"$n is not an object.")

case object TypeException extends SemanticException("Invalid type.")

case class UnresolvedSymbolException(s: String) extends SemanticException(s"Can not resolve symbol $s")

case object Base58DecodeException extends SemanticException("Base58 decode error.")

case class TypeMismatchException(t1: String, t2: String) extends SemanticException(s"Expected type $t1, got $t2.")

case object NestedCollectionException extends SemanticException("Nested collections are disallowed.")

case object DefaultBranchUndefinedException extends SemanticException("Match statement must contain default branch.")
