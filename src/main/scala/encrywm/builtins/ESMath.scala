package encrywm.builtins

import encrywm.frontend.ast.Ast
import encrywm.frontend.ast.Ast.OPERATOR._
import encrywm.frontend.ast.Ast.{EXPR, OPERATOR}
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
