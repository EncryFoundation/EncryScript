package encrywm.builtins

import encrywm.frontend.parser.Ast
import encrywm.frontend.semantics.BuiltInTypeSymbol

case class BuiltInType(name: String, symbol: BuiltInTypeSymbol, astType: Ast.TYPE)

object Builtins {

  import Ast.TYPE._
  import Ast.OPERATOR._

  val StaticBuiltInTypes: Set[BuiltInType] = Set(
    BuiltInType(Ast.TYPE.INT.name, BuiltInTypeSymbol(Ast.TYPE.INT.name), Ast.TYPE.INT),
    BuiltInType(Ast.TYPE.LONG.name, BuiltInTypeSymbol(Ast.TYPE.LONG.name), Ast.TYPE.LONG),
    BuiltInType(Ast.TYPE.DOUBLE.name, BuiltInTypeSymbol(Ast.TYPE.DOUBLE.name), Ast.TYPE.DOUBLE),
    BuiltInType(Ast.TYPE.FLOAT.name, BuiltInTypeSymbol(Ast.TYPE.FLOAT.name), Ast.TYPE.FLOAT),
    BuiltInType(Ast.TYPE.STRING.name, BuiltInTypeSymbol(Ast.TYPE.STRING.name), Ast.TYPE.STRING),
    BuiltInType(Ast.TYPE.BOOLEAN.name, BuiltInTypeSymbol(Ast.TYPE.BOOLEAN.name), Ast.TYPE.BOOLEAN),
    BuiltInType(Ast.TYPE.BYTE_VECTOR.name, BuiltInTypeSymbol(Ast.TYPE.BYTE_VECTOR.name), Ast.TYPE.BYTE_VECTOR),
    BuiltInType(Ast.TYPE.UNIT.name, BuiltInTypeSymbol(Ast.TYPE.UNIT.name), Ast.TYPE.UNIT)
  )

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
    (Mult, (FLOAT, FLOAT), FLOAT),
    // TODO: Complete the table.
  )
}
