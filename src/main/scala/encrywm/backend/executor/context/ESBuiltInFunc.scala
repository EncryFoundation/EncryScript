package encrywm.backend.executor.context

import encrywm.backend.executor.context.ESBuiltInFunc.EvalResult
import encrywm.backend.executor.error.ExecutionError
import encrywm.core.Types.ESType

case class ESBuiltInFunc(name: String,
                         args: IndexedSeq[(String, ESType)],
                         body: (Seq[(String, ESValue)]) => EvalResult) extends ESRuntimeComponent

object ESBuiltInFunc {

  val typeId: Byte = 3.toByte

  type EvalResult = Either[ExecutionError, Any]
}
