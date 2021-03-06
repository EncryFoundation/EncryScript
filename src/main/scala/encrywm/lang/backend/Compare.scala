package encrywm.lang.backend

import encrywm.lang.backend.env.ESObject
import encrywm.lang.backend.executor.error.UnsupportedOperationException

object Compare {

  def eq(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 == o2
      case (o1: Int, o2: Long) => o1 == o2
      case (o1: Int, o2: Float) => o1 == o2
      case (o1: Int, o2: Double) => o1 == o2
      case (o1: Long, o2: Int) => o1 == o2
      case (o1: Long, o2: Long) => o1 == o2
      case (o1: Long, o2: Float) => o1 == o2
      case (o1: Long, o2: Double) => o1 == o2
      case (o1: Float, o2: Int) => o1 == o2
      case (o1: Float, o2: Long) => o1 == o2
      case (o1: Float, o2: Float) => o1 == o2
      case (o1: Float, o2: Double) => o1 == o2
      case (o1: Double, o2: Int) => o1 == o2
      case (o1: Double, o2: Long) => o1 == o2
      case (o1: Double, o2: Float) => o1 == o2
      case (o1: Double, o2: Double) => o1 == o2
      case (o1: Boolean, o2: Boolean) => o1 == o2
      case (o1: Array[Byte], o2: Array[Byte]) => o1 sameElements o2
      case (o1: ESObject, o2: ESObject) => o1 == o2
      case _ => throw UnsupportedOperationException
    }
  }

  def gt(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 > o2
      case (o1: Int, o2: Long) => o1 > o2
      case (o1: Int, o2: Float) => o1 > o2
      case (o1: Int, o2: Double) => o1 > o2
      case (o1: Long, o2: Int) => o1 > o2
      case (o1: Long, o2: Long) => o1 > o2
      case (o1: Long, o2: Float) => o1 > o2
      case (o1: Long, o2: Double) => o1 > o2
      case (o1: Float, o2: Int) => o1 > o2
      case (o1: Float, o2: Long) => o1 > o2
      case (o1: Float, o2: Float) => o1 > o2
      case (o1: Float, o2: Double) => o1 > o2
      case (o1: Double, o2: Int) => o1 > o2
      case (o1: Double, o2: Long) => o1 > o2
      case (o1: Double, o2: Float) => o1 > o2
      case (o1: Double, o2: Double) => o1 > o2
      case _ => throw UnsupportedOperationException
    }
  }

  def gte(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 >= o2
      case (o1: Int, o2: Long) => o1 >= o2
      case (o1: Int, o2: Float) => o1 >= o2
      case (o1: Int, o2: Double) => o1 >= o2
      case (o1: Long, o2: Int) => o1 >= o2
      case (o1: Long, o2: Long) => o1 >= o2
      case (o1: Long, o2: Float) => o1 >= o2
      case (o1: Long, o2: Double) => o1 >= o2
      case (o1: Float, o2: Int) => o1 >= o2
      case (o1: Float, o2: Long) => o1 >= o2
      case (o1: Float, o2: Float) => o1 >= o2
      case (o1: Float, o2: Double) => o1 >= o2
      case (o1: Double, o2: Int) => o1 >= o2
      case (o1: Double, o2: Long) => o1 >= o2
      case (o1: Double, o2: Float) => o1 >= o2
      case (o1: Double, o2: Double) => o1 >= o2
      case _ => throw UnsupportedOperationException
    }
  }

  def lt(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 < o2
      case (o1: Int, o2: Long) => o1 < o2
      case (o1: Int, o2: Float) => o1 < o2
      case (o1: Int, o2: Double) => o1 < o2
      case (o1: Long, o2: Int) => o1 < o2
      case (o1: Long, o2: Long) => o1 < o2
      case (o1: Long, o2: Float) => o1 < o2
      case (o1: Long, o2: Double) => o1 < o2
      case (o1: Float, o2: Int) => o1 < o2
      case (o1: Float, o2: Long) => o1 < o2
      case (o1: Float, o2: Float) => o1 < o2
      case (o1: Float, o2: Double) => o1 < o2
      case (o1: Double, o2: Int) => o1 < o2
      case (o1: Double, o2: Long) => o1 < o2
      case (o1: Double, o2: Float) => o1 < o2
      case (o1: Double, o2: Double) => o1 < o2
      case _ => throw UnsupportedOperationException
    }
  }

  def lte(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 <= o2
      case (o1: Int, o2: Long) => o1 <= o2
      case (o1: Int, o2: Float) => o1 <= o2
      case (o1: Int, o2: Double) => o1 <= o2
      case (o1: Long, o2: Int) => o1 <= o2
      case (o1: Long, o2: Long) => o1 <= o2
      case (o1: Long, o2: Float) => o1 <= o2
      case (o1: Long, o2: Double) => o1 <= o2
      case (o1: Float, o2: Int) => o1 <= o2
      case (o1: Float, o2: Long) => o1 <= o2
      case (o1: Float, o2: Float) => o1 <= o2
      case (o1: Float, o2: Double) => o1 <= o2
      case (o1: Double, o2: Int) => o1 <= o2
      case (o1: Double, o2: Long) => o1 <= o2
      case (o1: Double, o2: Float) => o1 <= o2
      case (o1: Double, o2: Double) => o1 <= o2
      case _ => throw UnsupportedOperationException
    }
  }
}
