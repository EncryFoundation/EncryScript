package encrywm.backend.env

import encrywm.backend.env.ESBuiltInFunc.EvalResult
import encrywm.backend.executor.error.ExecutionError
import encrywm.lib.Types.ESType

case class ESBuiltInFunc(id: String,
                         args: IndexedSeq[(String, ESType)],
                         body: (Seq[(String, ESValue)]) => EvalResult) extends ESEnvComponent

object ESBuiltInFunc {

  val typeId: Byte = 3.toByte

  type EvalResult = Either[ExecutionError, Any]
}
