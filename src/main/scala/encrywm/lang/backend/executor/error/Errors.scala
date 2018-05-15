package encrywm.lang.backend.executor.error

class ExecutionException(s: String) extends Exception(s)

case class UnresolvedReferenceException(r: String) extends ExecutionException(s"Unresolved reference $r")

case class IsFunctionException(f: String) extends ExecutionException(s"$f is a function")

case class NotAFunctionException(f: String) extends ExecutionException(s"$f is not a function")

case class UnexpectedExpressionException(exp: String) extends ExecutionException(s"Unexpected expression $exp")

case object UnsupportedOperationException extends ExecutionException(s"Unsupported operation")

case object BuiltInFunctionExecException extends ExecutionException("Built-in function execution failed")

case object IllegalOperationException extends ExecutionException("Illegal operation")
