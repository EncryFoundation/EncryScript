package encrywm.core.environment.context

import encrywm.backend.env.{ESObject, ESValue}
import encrywm.core.Types.{ESByteVector, ESLong, ESState}

case class ESStateData(height: Long,
                       lastBlockTimestamp: Long,
                       stateDigest: Array[Byte])

class ESStateBuilder(d: ESStateData) extends EnvComponentBuilder {

  val instanceName: String = "state"

  override def asVal: ESValue = ESValue(instanceName, ESState)(build)

  override def build: ESObject = {
    val fields = Map(
      "height" -> ESValue("height", ESLong)(d.height),
      "lastBlockTimestamp" -> ESValue("lastBlockTimestamp", ESLong)(d.lastBlockTimestamp),
      "stateDigest" -> ESValue("stateDigest", ESByteVector)(d.stateDigest)
    )
    ESObject(ESState.identifier, fields)
  }
}
