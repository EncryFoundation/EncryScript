package encrywm.frontend.semantics.error

case class NameError(n: String) extends SemanticError(s"Name $n is undefined.")

case class AlreadyDefinedError(n: String) extends SemanticError(s"Name $n is already defined in scope.")

case class UnexpectedStatementError(s: String) extends SemanticError(s"Unexpected statement: $s.")

case object IllegalExprError extends SemanticError(s"Unexpected expression.")

case object IllegalOperandError extends SemanticError(s"Illegal operand type.")

case object ZeroDivisionError extends SemanticError(s"Zero division.")

case object MissedContextError extends SemanticError(s"Missed context for AST processing.")

case class WrongNumberOfArgumentsError(fn: String) extends SemanticError(s"Wrong number of arguments in $fn.")

case class NotAnObjectError(n: String) extends SemanticError(s"$n is not an object.")

case object TypeError extends SemanticError("Missed type.")

case class UnresolvedSymbolError(s: String) extends SemanticError(s"Can not resolve symbol $s")

case object Base58DecodeError extends SemanticError("Base58 decode error.")

case class TypeMismatchError(t1: String, t2: String) extends SemanticError(s"Expected type $t1, got $t2.")

case object NestedCollectionError extends SemanticError("Nested collections are disallowed.")

case object DefaultBranchUndefinedError extends SemanticError("Match statement must contain default branch.")
