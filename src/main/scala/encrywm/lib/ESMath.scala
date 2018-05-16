package encrywm.lib

import encrywm.ast.Ast
import encrywm.ast.Ast.OPERATOR._
import encrywm.ast.Ast.{EXPR, OPERATOR}
import encrywm.lang.frontend.semantics.exceptions.ZeroDivisionException

object ESMath {

  import Types._

  val BinaryOperationRuleset: Seq[(Ast.OPERATOR, (ESType, ESType), ESType)] = Seq(
    (Add, (ESInt, ESInt), ESInt),
    (Add, (ESInt, ESLong), ESLong),
    (Add, (ESLong, ESInt), ESLong),
    (Add, (ESLong, ESLong), ESLong),
    (Mult, (ESInt, ESInt), ESInt),
    (Mult, (ESInt, ESLong), ESLong),
    (Mult, (ESLong, ESInt), ESLong),
    (Mult, (ESLong, ESLong), ESLong),
    (Div, (ESInt, ESInt), ESInt),
    (Div, (ESInt, ESLong), ESLong),
    (Div, (ESLong, ESInt), ESLong),
    (Div, (ESLong, ESLong), ESLong),
  )

  def ensureZeroDivision(op: Ast.OPERATOR, operand2: Ast.EXPR): Unit = {
    operand2 match {
      case int: EXPR.IntConst
        if (op == OPERATOR.Div || op == OPERATOR.FloorDiv) && int.n == 0 =>
          throw ZeroDivisionException
      case long: EXPR.LongConst
        if (op == OPERATOR.Div || op == OPERATOR.FloorDiv) && long.n == 0L =>
          throw ZeroDivisionException
      case _ => // Do nothing.
    }
  }
}
