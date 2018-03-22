package encrywm.backend.executor

object Arith {

  def checkType[T](v: Any): T = v match {
    case t: T => t
    case _ => throw ExecutionError("Unexpected type")
  }

  def sum[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 + o2)
      case (o1: Long, o2: Int) => checkType[T](o1 + o2)
      case (o1: Int, o2: Long) => checkType[T](o1 + o2)
      case (o1: Long, o2: Long) => checkType[T](o1 + o2)
      case (o1: Double, o2: Int) => checkType[T](o1 + o2)
      case (o1: Int, o2: Double) => checkType[T](o1 + o2)
      case (o1: Double, o2: Int) => checkType[T](o1 + o2)
      case (o1: Double, o2: Double) => checkType[T](o1 + o2)
      case _ => throw ExecutionError("Unsupported operation")
    }
  }

  def mul[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 * o2)
      case (o1: Long, o2: Int) => checkType[T](o1 * o2)
      case (o1: Int, o2: Long) => checkType[T](o1 * o2)
      case (o1: Long, o2: Long) => checkType[T](o1 * o2)
      case (o1: Double, o2: Int) => checkType[T](o1 * o2)
      case (o1: Int, o2: Double) => checkType[T](o1 * o2)
      case (o1: Double, o2: Int) => checkType[T](o1 * o2)
      case (o1: Double, o2: Double) => checkType[T](o1 * o2)
      case _ => throw ExecutionError("Unsupported operation")
    }
  }

  def div[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 / o2)
      case (o1: Long, o2: Int) => checkType[T](o1 / o2)
      case (o1: Int, o2: Long) => checkType[T](o1 / o2)
      case (o1: Long, o2: Long) => checkType[T](o1 / o2)
      case (o1: Double, o2: Int) => checkType[T](o1 / o2)
      case (o1: Int, o2: Double) => checkType[T](o1 / o2)
      case (o1: Double, o2: Int) => checkType[T](o1 / o2)
      case (o1: Double, o2: Double) => checkType[T](o1 / o2)
      case _ => throw ExecutionError("Unsupported operation")
    }
  }
}
