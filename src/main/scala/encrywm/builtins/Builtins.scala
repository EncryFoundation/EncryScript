package encrywm.builtins

import encrywm.frontend.parser.Ast

object Builtins {

  val builtinTypes: Seq[String] = Seq(
    Ast.TYPE.INT.name,
    Ast.TYPE.LONG.name,
    Ast.TYPE.DOUBLE.name,
    Ast.TYPE.FLOAT.name,
    Ast.TYPE.STRING.name,
    Ast.TYPE.BOOLEAN.name,
    Ast.TYPE.BYTE_VECTOR.name,
    Ast.TYPE.UNIT.name,
    "true",
    "false"
  )
}
