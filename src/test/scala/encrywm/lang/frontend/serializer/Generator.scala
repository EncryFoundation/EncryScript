package encrywm.lang.frontend.serializer

import encrywm.ast.Ast.BOOL_OP.And
import encrywm.ast.Ast._
import encrywm.ast.Ast.EXPR.{BoolOp, IntConst}
import encrywm.ast.Ast.STMT.FunctionDef

object Generator {

  def generateEXPRs(count: Int): List[EXPR] =
    (0 until count).foldLeft(List[EXPR]()){
      (seq, i) => seq :+ BoolOp(And, List(IntConst(2), IntConst(3)))
    }

  def generateBoolOp: EXPR.BoolOp = BoolOp(And, List(IntConst(2), IntConst(3)))

  def generateSTMTs(count: Int): List[STMT] =
    (0 until count).foldLeft(List[STMT]()){
      (seq, i) => seq :+ FunctionDef(
        Identifier(s"count $count"),
        Arguments(List()),
        generateSTMTs(count-1),
        Identifier("mnopqrst")
      )
    }

  def generateContract: TREE_ROOT.Contract = TREE_ROOT.Contract(generateSTMTs(2))

  def generateFunctionDef: STMT.FunctionDef = FunctionDef(
    Identifier("23456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"),
    Arguments(List()),
    List(),
    Identifier("23456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
  )
}
