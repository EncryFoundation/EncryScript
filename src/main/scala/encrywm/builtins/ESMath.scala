package encrywm.builtins

import encrywm.ast.Ast
import encrywm.ast.Ast.OPERATOR._
import encrywm.ast.Ast.{EXPR, OPERATOR}
import encrywm.backend.evaluator.EvaluationError
import encrywm.frontend.semantics.error.ZeroDivisionError

object ESMath {

  import Types._

  val BinaryOperationResults: Seq[(Ast.OPERATOR, (TYPE, TYPE), TYPE)] = Seq(
    (Add, (INT, INT), INT),
    (Add, (INT, LONG), LONG),
    (Add, (LONG, INT), LONG),
    (Add, (LONG, LONG), LONG),
    (Add, (INT, FLOAT), FLOAT),
    (Add, (FLOAT, INT), FLOAT),
    (Add, (FLOAT, FLOAT), FLOAT),
    (Add, (DOUBLE, INT), DOUBLE),
    (Add, (INT, DOUBLE), DOUBLE),
    (Add, (DOUBLE, DOUBLE), DOUBLE),
    (Mult, (INT, INT), INT),
    (Mult, (INT, LONG), LONG),
    (Mult, (LONG, INT), LONG),
    (Mult, (LONG, LONG), LONG),
    (Mult, (INT, FLOAT), FLOAT),
    (Mult, (FLOAT, INT), FLOAT),
    (Mult, (FLOAT, FLOAT), FLOAT)
    // TODO: Complete the table.
  )

  def sum[T](op1: Any, op2: Any): T = {
    def checkType(v: Any): T = v match {
      case t: T => t
      case _ => throw EvaluationError("Unexpected type")
    }
    (op1, op2) match {
      case (o1: Int, o2: Int) => checkType(o1 + o2)
    }
  }

  def ensureZeroDivision(op: Ast.OPERATOR, operand2: Ast.EXPR): Unit = {
    operand2 match {
      case int: EXPR.IntConst
        if (op == OPERATOR.Div || op == OPERATOR.FloorDiv) && int.n == 0 =>
          throw ZeroDivisionError
      case long: EXPR.LongConst
        if (op == OPERATOR.Div || op == OPERATOR.FloorDiv) && long.n == 0L =>
          throw ZeroDivisionError
      case _ => // Do nothing.
    }
  }
}
