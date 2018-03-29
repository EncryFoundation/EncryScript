package encrywm.backend

import encrywm.backend.executor.error.UnsupportedOperationError

object Arith {

  def checkType[T](v: Any): T = v match {
    case t: T@unchecked => t
    case _ => throw UnsupportedOperationError
  }

  def sum[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 + o2)
      case (o1: Int, o2: Float) => checkType[T](o1 + o2)
      case (o1: Int, o2: Double) => checkType[T](o1 + o2)
      case (o1: Int, o2: Long) => checkType[T](o1 + o2)
      case (o1: Float, o2: Float) => checkType[T](o1 + o2)
      case (o1: Float, o2: Int) => checkType[T](o1 + o2)
      case (o1: Float, o2: Double) => checkType[T](o1 + o2)
      case (o1: Float, o2: Long) => checkType[T](o1 + o2)
      case (o1: Double, o2: Float) => checkType[T](o1 + o2)
      case (o1: Double, o2: Int) => checkType[T](o1 + o2)
      case (o1: Double, o2: Double) => checkType[T](o1 + o2)
      case (o1: Double, o2: Long) => checkType[T](o1 + o2)
      case (o1: Long, o2: Float) => checkType[T](o1 + o2)
      case (o1: Long, o2: Int) => checkType[T](o1 + o2)
      case (o1: Long, o2: Double) => checkType[T](o1 + o2)
      case (o1: Long, o2: Long) => checkType[T](o1 + o2)
      case _ => throw UnsupportedOperationError
    }
  }

  def mul[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 * o2)
      case (o1: Int, o2: Float) => checkType[T](o1 * o2)
      case (o1: Int, o2: Double) => checkType[T](o1 * o2)
      case (o1: Int, o2: Long) => checkType[T](o1 * o2)
      case (o1: Float, o2: Float) => checkType[T](o1 * o2)
      case (o1: Float, o2: Int) => checkType[T](o1 * o2)
      case (o1: Float, o2: Double) => checkType[T](o1 * o2)
      case (o1: Float, o2: Long) => checkType[T](o1 * o2)
      case (o1: Double, o2: Float) => checkType[T](o1 * o2)
      case (o1: Double, o2: Int) => checkType[T](o1 * o2)
      case (o1: Double, o2: Double) => checkType[T](o1 * o2)
      case (o1: Double, o2: Long) => checkType[T](o1 * o2)
      case (o1: Long, o2: Float) => checkType[T](o1 * o2)
      case (o1: Long, o2: Int) => checkType[T](o1 * o2)
      case (o1: Long, o2: Double) => checkType[T](o1 * o2)
      case (o1: Long, o2: Long) => checkType[T](o1 * o2)
      case _ => throw UnsupportedOperationError
    }
  }

  def div[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 / o2)
      case (o1: Int, o2: Float) => checkType[T](o1 / o2)
      case (o1: Int, o2: Double) => checkType[T](o1 / o2)
      case (o1: Int, o2: Long) => checkType[T](o1 / o2)
      case (o1: Float, o2: Float) => checkType[T](o1 / o2)
      case (o1: Float, o2: Int) => checkType[T](o1 / o2)
      case (o1: Float, o2: Double) => checkType[T](o1 / o2)
      case (o1: Float, o2: Long) => checkType[T](o1 / o2)
      case (o1: Double, o2: Float) => checkType[T](o1 / o2)
      case (o1: Double, o2: Int) => checkType[T](o1 / o2)
      case (o1: Double, o2: Double) => checkType[T](o1 / o2)
      case (o1: Double, o2: Long) => checkType[T](o1 / o2)
      case (o1: Long, o2: Float) => checkType[T](o1 / o2)
      case (o1: Long, o2: Int) => checkType[T](o1 / o2)
      case (o1: Long, o2: Double) => checkType[T](o1 / o2)
      case (o1: Long, o2: Long) => checkType[T](o1 / o2)
      case _ => throw UnsupportedOperationError
    }
  }
}
