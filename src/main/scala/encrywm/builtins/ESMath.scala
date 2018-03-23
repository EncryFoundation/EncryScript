package encrywm.builtins

import encrywm.ast.Ast
import encrywm.ast.Ast.OPERATOR._
import encrywm.ast.Ast.{EXPR, OPERATOR}
import encrywm.frontend.semantics.error.ZeroDivisionError

object ESMath {

  import Types._

  val BinaryOperationResults: Seq[(Ast.OPERATOR, (TYPE, TYPE), TYPE)] = Seq(
    (Add, (INT, INT), INT),
    (Add, (INT, LONG), LONG),
    (Add, (INT, FLOAT), FLOAT),
    (Add, (INT, DOUBLE), DOUBLE),
    (Add, (LONG, INT), LONG),
    (Add, (LONG, LONG), LONG),
    (Add, (LONG, FLOAT), FLOAT),
    (Add, (LONG, DOUBLE), DOUBLE),
    (Add, (FLOAT, INT), FLOAT),
    (Add, (FLOAT, LONG), FLOAT),
    (Add, (FLOAT, FLOAT), FLOAT),
    (Add, (FLOAT, DOUBLE), DOUBLE),
    (Add, (DOUBLE, INT), DOUBLE),
    (Add, (DOUBLE, LONG), DOUBLE),
    (Add, (DOUBLE, FLOAT), DOUBLE),
    (Add, (DOUBLE, DOUBLE), DOUBLE),
    (Mult, (INT, INT), INT),
    (Mult, (INT, LONG), LONG),
    (Mult, (INT, FLOAT), FLOAT),
    (Mult, (INT, DOUBLE), DOUBLE),
    (Mult, (LONG, INT), LONG),
    (Mult, (LONG, LONG), LONG),
    (Mult, (LONG, FLOAT), FLOAT),
    (Mult, (LONG, DOUBLE), DOUBLE),
    (Mult, (FLOAT, INT), FLOAT),
    (Mult, (FLOAT, LONG), FLOAT),
    (Mult, (FLOAT, FLOAT), FLOAT),
    (Mult, (FLOAT, DOUBLE), DOUBLE),
    (Mult, (DOUBLE, INT), DOUBLE),
    (Mult, (DOUBLE, LONG), DOUBLE),
    (Mult, (DOUBLE, FLOAT), DOUBLE),
    (Mult, (DOUBLE, DOUBLE), DOUBLE),
    (Div, (INT, INT), INT),
    (Div, (INT, LONG), LONG),
    (Div, (INT, FLOAT), FLOAT),
    (Div, (INT, DOUBLE), DOUBLE),
    (Div, (LONG, INT), LONG),
    (Div, (LONG, LONG), LONG),
    (Div, (LONG, FLOAT), FLOAT),
    (Div, (LONG, DOUBLE), DOUBLE),
    (Div, (FLOAT, INT), FLOAT),
    (Div, (FLOAT, LONG), FLOAT),
    (Div, (FLOAT, FLOAT), FLOAT),
    (Div, (FLOAT, DOUBLE), DOUBLE),
    (Div, (DOUBLE, INT), DOUBLE),
    (Div, (DOUBLE, LONG), DOUBLE),
    (Div, (DOUBLE, FLOAT), DOUBLE),
    (Div, (DOUBLE, DOUBLE), DOUBLE)
  )

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
