package encrywm.lang.backend.executor.error

class RuntimeException(s: String) extends Exception(s)

case class UnresolvedReferenceException(r: String) extends RuntimeException(s"Unresolved reference $r")

case class IsFunctionException(f: String) extends RuntimeException(s"$f is a function")

case class NotAFunctionException(f: String) extends RuntimeException(s"$f is not a function")

case class UnexpectedExpressionException(exp: String) extends RuntimeException(s"Unexpected expression $exp")

case object UnsupportedOperationException extends RuntimeException(s"Unsupported operation")

case object BuiltInFunctionExecException extends RuntimeException("Built-in function execution failed")

case object IllegalOperationException extends RuntimeException("Illegal operation")
