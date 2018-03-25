package encrywm.backend.executor.error

case class UnresolvedReferenceError(r: String) extends ExecutionError(s"Unresolved reference $r")

case class IsFunctionError(f: String) extends ExecutionError(s"$f is a function")

case class NotAFunctionError(f: String) extends ExecutionError(s"$f is not a function")

case class UnexpectedExpressionError(exp: String) extends ExecutionError(s"Unexpected expression $exp")

case object UnsupportedOperationError extends ExecutionError(s"Unsupported operation")

case object BuiltInFunctionExecError extends ExecutionError("Built-in function execution error")

case object IllegalOperationError extends ExecutionError("Illegal operation")
