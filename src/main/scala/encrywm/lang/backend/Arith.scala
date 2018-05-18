package encrywm.lang.backend

import encrywm.lang.backend.executor.error.UnsupportedOperationException

object Arith {

  def checkType[T](v: Any): T = v match {
    case t: T@unchecked => t
    case _ => throw UnsupportedOperationException
  }

  def add[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 + o2)
      case (o1: Int, o2: Long) => checkType[T](o1 + o2)
      case (o1: Long, o2: Int) => checkType[T](o1 + o2)
      case (o1: Long, o2: Long) => checkType[T](o1 + o2)
      case _ => throw UnsupportedOperationException
    }
  }

  def sub[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 - o2)
      case (o1: Int, o2: Long) => checkType[T](o1 - o2)
      case (o1: Long, o2: Int) => checkType[T](o1 - o2)
      case (o1: Long, o2: Long) => checkType[T](o1 - o2)
      case _ => throw UnsupportedOperationException
    }
  }

  def mul[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 * o2)
      case (o1: Int, o2: Long) => checkType[T](o1 * o2)
      case (o1: Long, o2: Int) => checkType[T](o1 * o2)
      case (o1: Long, o2: Long) => checkType[T](o1 * o2)
      case _ => throw UnsupportedOperationException
    }
  }

  def div[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 / o2)
      case (o1: Int, o2: Long) => checkType[T](o1 / o2)
      case (o1: Long, o2: Int) => checkType[T](o1 / o2)
      case (o1: Long, o2: Long) => checkType[T](o1 / o2)
      case _ => throw UnsupportedOperationException
    }
  }

  def mod[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType[T](o1 % o2)
      case (o1: Int, o2: Long) => checkType[T](o1 % o2)
      case (o1: Long, o2: Int) => checkType[T](o1 % o2)
      case (o1: Long, o2: Long) => checkType[T](o1 % o2)
      case _ => throw UnsupportedOperationException
    }
  }
}
