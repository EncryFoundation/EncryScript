package encrywm.backend.interpreter.context

import encrywm.builtins.Types.TYPE
import encrywm.frontend.ast.Ast.STMT

case class ESFunc(name: String, args: IndexedSeq[(String, TYPE)], returnType: TYPE, body: Seq[STMT]) extends ESCtxComponent

object ESFunc {
  val typeId: Byte = 1.toByte
}
