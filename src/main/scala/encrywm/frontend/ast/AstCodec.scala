package encrywm.frontend.ast

import encrywm.frontend.ast.Ast._
import scodec.{Codec, codecs}
import scodec.codecs.{Discriminated, uint8}

object AstCodec {

  import codecs.implicits._

  implicit def dSt = Discriminated[STMT, Int](uint8)
  implicit def dExpr = dSt.bind[STMT.Expr](0)

  implicit def dEx = Discriminated[EXPR, Int](uint8)
  implicit def dIntConst = dEx.bind[EXPR.IntConst](0)
  implicit def dLongConst = dEx.bind[EXPR.LongConst](1)
  implicit def dBinOp = dEx.bind[EXPR.BinOp](2)

  implicit def dOp = Discriminated[OPERATOR, Int](uint8)
  implicit def dSum = dOp.bind[OPERATOR.Add.type](0)
}
