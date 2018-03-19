package encrywm.frontend.ast

import encrywm.frontend.ast.Ast._
import scodec.{Codec, codecs}
import scodec.codecs.{Discriminated, uint4}

object AstCodec {

  import codecs.implicits._

  implicit def dRoot = Discriminated[TREE_ROOT, Int](uint4)
  implicit def dCon = dRoot.bind[TREE_ROOT.Contract](0)
  implicit def dRExp = dRoot.bind[TREE_ROOT.Expression](1)

  implicit def dT = Discriminated[TYPE, Int](uint4)
  implicit def dUnit = dT.bind[TYPE.UNIT.type](0)
  implicit def dBool = dT.bind[TYPE.BOOLEAN.type](1)
  implicit def dInt = dT.bind[TYPE.INT.type](2)
  implicit def dLong = dT.bind[TYPE.LONG.type](3)
  implicit def dFloat = dT.bind[TYPE.FLOAT.type](4)
  implicit def dDouble = dT.bind[TYPE.DOUBLE.type](5)
  implicit def dStr = dT.bind[TYPE.STRING.type](6)
  implicit def dBytes = dT.bind[TYPE.BYTE_VECTOR.type](7)
  implicit def dList = dT.bind[TYPE.LIST](8)
  implicit def dDict = dT.bind[TYPE.DICT](9)
  implicit def dOpt = dT.bind[TYPE.OPTION](10)
  implicit def dTr = dT.bind[TYPE.TYPE_REF](11)

  implicit def dSt = Discriminated[STMT, Int](uint4)
  implicit def dFnDef = dSt.bind[STMT.FunctionDef](0)
  implicit def dRet = dSt.bind[STMT.Return](1)
  implicit def dAsg = dSt.bind[STMT.Assign](2)
  implicit def dAugAsg = dSt.bind[STMT.AugAssign](3)
  implicit def dFor = dSt.bind[STMT.For](4)
  implicit def dIf = dSt.bind[STMT.If](5)
  implicit def dAsrt = dSt.bind[STMT.Assert](6)
  implicit def dExpSt = dSt.bind[STMT.Expr](7)
  implicit def dUnl = dSt.bind[STMT.Unlock.type](8)
  implicit def dHalt = dSt.bind[STMT.Halt.type](9)

  implicit def dEx = Discriminated[EXPR, Int](uint4)
  implicit def dIntConst = dEx.bind[EXPR.IntConst](0)
  implicit def dLongConst = dEx.bind[EXPR.LongConst](1)
  implicit def dBinOp = dEx.bind[EXPR.BinOp](2)

  implicit def dOp = Discriminated[OPERATOR, Int](uint4)
  implicit def dSum = dOp.bind[OPERATOR.Add.type](0)
}
