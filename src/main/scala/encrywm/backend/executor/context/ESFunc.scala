package encrywm.backend.executor.context

import encrywm.builtins.Types.ESType
import encrywm.ast.Ast.STMT

case class ESFunc(name: String,
                  args: IndexedSeq[(String, ESType)],
                  returnType: ESType,
                  body: Seq[STMT]) extends ESRuntimeComponent

object ESFunc {
  val typeId: Byte = 1.toByte
}
