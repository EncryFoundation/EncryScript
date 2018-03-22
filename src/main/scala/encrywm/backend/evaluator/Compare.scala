package encrywm.backend.evaluator

object Compare {

  def eq(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 == o2
      case (o1: Long, o2: Int) => o1 == o2
      case (o1: Int, o2: Long) => o1 == o2
      case (o1: Long, o2: Long) => o1 == o2
      case (o1: Double, o2: Int) => o1 == o2
      case (o1: Int, o2: Double) => o1 == o2
      case (o1: Double, o2: Double) => o1 == o2
      case (o1: Float, o2: Int) => o1 == o2
      case (o1: Int, o2: Float) => o1 == o2
      case _ => throw EvaluationError("Unsupported operation")
    }
  }

  def gt(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 > o2
      case (o1: Long, o2: Int) => o1 > o2
      case (o1: Int, o2: Long) => o1 > o2
      case (o1: Long, o2: Long) => o1 > o2
      case (o1: Double, o2: Int) => o1 > o2
      case (o1: Int, o2: Double) => o1 > o2
      case (o1: Double, o2: Double) => o1 > o2
      case (o1: Float, o2: Int) => o1 > o2
      case (o1: Int, o2: Float) => o1 > o2
      case _ => throw EvaluationError("Unsupported operation")
    }
  }

  def gte(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 >= o2
      case (o1: Long, o2: Int) => o1 >= o2
      case (o1: Int, o2: Long) => o1 >= o2
      case (o1: Long, o2: Long) => o1 >= o2
      case (o1: Double, o2: Int) => o1 >= o2
      case (o1: Int, o2: Double) => o1 >= o2
      case (o1: Double, o2: Double) => o1 >= o2
      case (o1: Float, o2: Int) => o1 >= o2
      case (o1: Int, o2: Float) => o1 >= o2
      case _ => throw EvaluationError("Unsupported operation")
    }
  }

  def lt(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 < o2
      case (o1: Long, o2: Int) => o1 < o2
      case (o1: Int, o2: Long) => o1 < o2
      case (o1: Long, o2: Long) => o1 < o2
      case (o1: Double, o2: Int) => o1 < o2
      case (o1: Int, o2: Double) => o1 < o2
      case (o1: Double, o2: Double) => o1 < o2
      case (o1: Float, o2: Int) => o1 < o2
      case (o1: Int, o2: Float) => o1 < o2
      case _ => throw EvaluationError("Unsupported operation")
    }
  }

  def lte(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Int, o2: Int) => o1 <= o2
      case (o1: Long, o2: Int) => o1 <= o2
      case (o1: Int, o2: Long) => o1 <= o2
      case (o1: Long, o2: Long) => o1 <= o2
      case (o1: Double, o2: Int) => o1 <= o2
      case (o1: Int, o2: Double) => o1 <= o2
      case (o1: Double, o2: Double) => o1 <= o2
      case (o1: Float, o2: Int) => o1 <= o2
      case (o1: Int, o2: Float) => o1 <= o2
      case _ => throw EvaluationError("Unsupported operation")
    }
  }
}
