package frontend.serializer

import encrywm.frontend.ast.Ast.BOOL_OP.And
import encrywm.frontend.ast.Ast.{Arguments, EXPR, Identifier, STMT}
import encrywm.frontend.ast.Ast.EXPR.{BoolOp, IntConst}
import encrywm.frontend.ast.Ast.STMT.FunctionDef

object Generator {

  def generateEXPRs(count: Int): Seq[EXPR] =
    (0 until count).foldLeft(Seq[EXPR]()){
      (seq, i) => seq :+ BoolOp(And, Seq(IntConst(2), IntConst(3)))
    }

  def generateBoolOp: EXPR.BoolOp = BoolOp(And, Seq(IntConst(2), IntConst(3)))

  def generateSTMTs(count: Int): Seq[STMT] =
    (0 until count).foldLeft(Seq[STMT]()){
      (seq, i) => seq :+ FunctionDef(
        Identifier(s"count${count}"),
        Arguments(Seq()),
        generateSTMTs(count-1),
        Identifier("mnopqrstu")
      )
    }

  def generateFunctionDef: STMT.FunctionDef = FunctionDef(
    Identifier("23456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"),
    Arguments(Seq()),
    Seq(),
    Identifier("23456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
  )

}
