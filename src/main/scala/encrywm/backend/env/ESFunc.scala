package encrywm.backend.env

import encrywm.lib.Types.ESType
import encrywm.ast.Ast.STMT

case class ESFunc(id: String,
                  args: IndexedSeq[(String, ESType)],
                  returnType: ESType,
                  body: Seq[STMT]) extends ESEnvComponent

object ESFunc {
  val typeId: Byte = 1.toByte
}
