package encrywm.backend.executor.env

import encrywm.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types.{ESByteVector, ESLong, ESState}
import encrywm.lib.predef.env.ESEnvConvertable

case class ESStateData(height: Long,
                       lastBlockTimestamp: Long,
                       stateDigest: Array[Byte])

class ESStateBuilder(d: ESStateData) extends ESEnvConvertable {

  val instanceName: String = "state"

  override val esType: Types.ESProduct = ESState

  override def asVal: ESValue = ESValue(instanceName, ESState)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "height" -> ESValue("height", ESLong)(d.height),
      "lastBlockTimestamp" -> ESValue("lastBlockTimestamp", ESLong)(d.lastBlockTimestamp),
      "stateDigest" -> ESValue("stateDigest", ESByteVector)(d.stateDigest)
    )
    ESObject(ESState.ident, fields, esType)
  }
}
