package encrywm.lang.frontend.semantics.exceptions

class SemanticException(m: String, codeExample: String) extends Exception(m.concat(s" In '$codeExample'"))

case class NameException(n: String, codeExample: String) extends SemanticException(s"Name $n is undefined.", codeExample: String)

case class AlreadyDefinedException(n: String, codeExample: String) extends SemanticException(s"Name $n is already defined in scope.", codeExample: String)

case class UnexpectedStatementException(s: String, codeExample: String) extends SemanticException(s"Unexpected statement: $s.", codeExample: String)

case class IllegalExprException(codeExample: String) extends SemanticException(s"Unexpected expression.", codeExample: String)

case class IllegalOperandException(codeExample: String) extends SemanticException(s"Illegal operand type.", codeExample: String)

case class ZeroDivisionException(codeExample: String) extends SemanticException(s"Zero division.", codeExample: String)

case class MissedContextException(codeExample: String) extends SemanticException(s"Missed context for AST processing.", codeExample: String)

case class WrongNumberOfArgumentsException(fn: String, codeExample: String) extends SemanticException(s"Wrong number of arguments in $fn.", codeExample: String)

case class UnapplicableFunctionException(fn: String, coll: String, codeExample: String) extends SemanticException(s"$fn is unapplicable to $coll", codeExample: String)

case class NotAnObjectException(n: String, codeExample: String) extends SemanticException(s"$n is not an object.", codeExample: String)

case class TypeException(codeExample: String) extends SemanticException("Missed type.", codeExample: String)

case class UnresolvedSymbolException(s: String, codeExample: String) extends SemanticException(s"Can not resolve symbol $s", codeExample: String)

case class Base58DecodeException(codeExample: String) extends SemanticException("Base58 decode error.", codeExample: String)

case class TypeMismatchException(t1: String, t2: String, codeExample: String) extends SemanticException(s"Expected type $t1, got $t2.", codeExample: String)

case class NestedCollectionException(codeExample: String) extends SemanticException("Nested collections are disallowed.", codeExample: String)

case class DefaultBranchUndefinedException(codeExample: String) extends SemanticException("Match statement must contain default branch.", codeExample: String)

case class IllegalUnlockIfScopeException(codeExample: String) extends SemanticException("'Unlock if' statement appeared within function scope.", codeExample: String)
