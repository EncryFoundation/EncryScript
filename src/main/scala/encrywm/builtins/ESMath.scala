package encrywm.builtins

import encrywm.frontend.parser.Ast
import encrywm.frontend.parser.Ast.OPERATOR.{Add, Mult}
import encrywm.frontend.parser.Ast.TYPE.{DOUBLE, FLOAT, INT, LONG}
import encrywm.frontend.parser.Ast.{EXPR, OPERATOR}
import encrywm.frontend.semantics.ZeroDivisionError

object ESMath {

  val BinaryOperationResults: Seq[(Ast.OPERATOR, (Ast.TYPE, Ast.TYPE), Ast.TYPE)] = Seq(
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
