package encrywm.lib

import encrywm.ast.Ast
import encrywm.ast.Ast.OPERATOR._
import encrywm.ast.Ast.{EXPR, OPERATOR}
import encrywm.frontend.semantics.error.ZeroDivisionError

object ESMath {

  import Types._

  val BinaryOperationResults: Seq[(Ast.OPERATOR, (ESType, ESType), ESType)] = Seq(
    (Add, (ESInt, ESInt), ESInt),
    (Add, (ESInt, ESLong), ESLong),
    (Add, (ESInt, FLOAT), FLOAT),
    (Add, (ESInt, DOUBLE), DOUBLE),
    (Add, (ESLong, ESInt), ESLong),
    (Add, (ESLong, ESLong), ESLong),
    (Add, (ESLong, FLOAT), FLOAT),
    (Add, (ESLong, DOUBLE), DOUBLE),
    (Add, (FLOAT, ESInt), FLOAT),
    (Add, (FLOAT, ESLong), FLOAT),
    (Add, (FLOAT, FLOAT), FLOAT),
    (Add, (FLOAT, DOUBLE), DOUBLE),
    (Add, (DOUBLE, ESInt), DOUBLE),
    (Add, (DOUBLE, ESLong), DOUBLE),
    (Add, (DOUBLE, FLOAT), DOUBLE),
    (Add, (DOUBLE, DOUBLE), DOUBLE),
    (Mult, (ESInt, ESInt), ESInt),
    (Mult, (ESInt, ESLong), ESLong),
    (Mult, (ESInt, FLOAT), FLOAT),
    (Mult, (ESInt, DOUBLE), DOUBLE),
    (Mult, (ESLong, ESInt), ESLong),
    (Mult, (ESLong, ESLong), ESLong),
    (Mult, (ESLong, FLOAT), FLOAT),
    (Mult, (ESLong, DOUBLE), DOUBLE),
    (Mult, (FLOAT, ESInt), FLOAT),
    (Mult, (FLOAT, ESLong), FLOAT),
    (Mult, (FLOAT, FLOAT), FLOAT),
    (Mult, (FLOAT, DOUBLE), DOUBLE),
    (Mult, (DOUBLE, ESInt), DOUBLE),
    (Mult, (DOUBLE, ESLong), DOUBLE),
    (Mult, (DOUBLE, FLOAT), DOUBLE),
    (Mult, (DOUBLE, DOUBLE), DOUBLE),
    (Div, (ESInt, ESInt), ESInt),
    (Div, (ESInt, ESLong), ESLong),
    (Div, (ESInt, FLOAT), FLOAT),
    (Div, (ESInt, DOUBLE), DOUBLE),
    (Div, (ESLong, ESInt), ESLong),
    (Div, (ESLong, ESLong), ESLong),
    (Div, (ESLong, FLOAT), FLOAT),
    (Div, (ESLong, DOUBLE), DOUBLE),
    (Div, (FLOAT, ESInt), FLOAT),
    (Div, (FLOAT, ESLong), FLOAT),
    (Div, (FLOAT, FLOAT), FLOAT),
    (Div, (FLOAT, DOUBLE), DOUBLE),
    (Div, (DOUBLE, ESInt), DOUBLE),
    (Div, (DOUBLE, ESLong), DOUBLE),
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
