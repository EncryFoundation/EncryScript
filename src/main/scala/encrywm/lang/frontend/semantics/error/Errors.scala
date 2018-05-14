package encrywm.lang.frontend.semantics.error

case class NameError(n: String, codeExample: String) extends SemanticError(s"Name $n is undefined.", codeExample: String)

case class AlreadyDefinedError(n: String, codeExample: String) extends SemanticError(s"Name $n is already defined in scope.", codeExample: String)

case class UnexpectedStatementError(s: String, codeExample: String) extends SemanticError(s"Unexpected statement: $s.", codeExample: String)

case class IllegalExprError(codeExample: String) extends SemanticError(s"Unexpected expression.", codeExample: String)

case class IllegalOperandError(codeExample: String) extends SemanticError(s"Illegal operand type.", codeExample: String)

case class ZeroDivisionError(codeExample: String) extends SemanticError(s"Zero division.", codeExample: String)

case class MissedContextError(codeExample: String) extends SemanticError(s"Missed context for AST processing.", codeExample: String)

case class WrongNumberOfArgumentsError(fn: String, codeExample: String) extends SemanticError(s"Wrong number of arguments in $fn.", codeExample: String)

case class UnapplicableFunctionError(fn: String, coll: String, codeExample: String) extends SemanticError(s"$fn is unapplicable to $coll", codeExample: String)

case class NotAnObjectError(n: String, codeExample: String) extends SemanticError(s"$n is not an object.", codeExample: String)

case class TypeError(codeExample: String) extends SemanticError("Missed type.", codeExample: String)

case class UnresolvedSymbolError(s: String, codeExample: String) extends SemanticError(s"Can not resolve symbol $s", codeExample: String)

case class Base58DecodeError(codeExample: String) extends SemanticError("Base58 decode error.", codeExample: String)

case class TypeMismatchError(t1: String, t2: String, codeExample: String) extends SemanticError(s"Expected type $t1, got $t2.", codeExample: String)

case class NestedCollectionError(codeExample: String) extends SemanticError("Nested collections are disallowed.", codeExample: String)

case class DefaultBranchUndefinedError(codeExample: String) extends SemanticError("Match statement must contain default branch.", codeExample: String)
