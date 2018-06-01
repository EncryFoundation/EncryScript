package encrywm.lang.frontend.semantics.exceptions

import encrywm.ast.Ast.AST_NODE

class SemanticException(m: String, codeExample: AST_NODE) extends Exception(m.concat(s" In '$codeExample'"))

case class NameException(n: String, codeExample: AST_NODE) extends SemanticException(s"Name $n is undefined.", codeExample: AST_NODE)

case class AlreadyDefinedException(n: String, codeExample: AST_NODE) extends SemanticException(s"Name $n is already defined in scope.", codeExample: AST_NODE)

case class UnexpectedStatementException(s: String, codeExample: AST_NODE) extends SemanticException(s"Unexpected statement: $s.", codeExample: AST_NODE)

case class IllegalExprException(codeExample: AST_NODE) extends SemanticException(s"Unexpected expression.", codeExample: AST_NODE)

case class IllegalOperandException(codeExample: AST_NODE) extends SemanticException(s"Illegal operand type.", codeExample: AST_NODE)

case class ZeroDivisionException(codeExample: AST_NODE) extends SemanticException(s"Zero division.", codeExample: AST_NODE)

case class MissedContextException(codeExample: AST_NODE) extends SemanticException(s"Missed context for AST processing.", codeExample: AST_NODE)

case class WrongNumberOfArgumentsException(fn: String, codeExample: AST_NODE) extends SemanticException(s"Wrong number of arguments in $fn.", codeExample: AST_NODE)

case class UnapplicableFunctionException(fn: String, coll: String, codeExample: AST_NODE) extends SemanticException(s"$fn is unapplicable to $coll", codeExample: AST_NODE)

case class NotAnObjectException(n: String, codeExample: AST_NODE) extends SemanticException(s"$n is not an object.", codeExample: AST_NODE)

case class TypeException(codeExample: AST_NODE) extends SemanticException("Missed type.", codeExample: AST_NODE)

case class UnresolvedSymbolException(s: String, codeExample: AST_NODE) extends SemanticException(s"Can not resolve symbol $s", codeExample: AST_NODE)

case class Base58DecodeException(codeExample: AST_NODE) extends SemanticException("Base58 decode error.", codeExample: AST_NODE)

case class TypeMismatchException(t1: String, t2: String, codeExample: AST_NODE) extends SemanticException(s"Expected type $t1, got $t2.", codeExample: AST_NODE)

case class NestedCollectionException(codeExample: AST_NODE) extends SemanticException("Nested collections are disallowed.", codeExample: AST_NODE)

case class DefaultBranchUndefinedException(codeExample: AST_NODE) extends SemanticException("Match statement must contain default branch.", codeExample: AST_NODE)

case class IllegalUnlockIfScopeException(codeExample: AST_NODE) extends SemanticException("'Unlock if' statement appeared within function scope.", codeExample: AST_NODE)
