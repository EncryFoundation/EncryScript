package encrywm.core.environment.context

import encrywm.backend.executor.context.{ESObject, ESValue}
import encrywm.core.Types.{ESByteVector, ESLong, ESState}

case class ESStateBuilder(height: Long,
                          lastBlockTimestamp: Long,
                          stateDigest: Array[Byte]) {

  val instanceName: String = "state"

  def asVal: ESValue = ESValue(instanceName, ESState)(nativeObject)

  def nativeObject: ESObject = {
    val fields = Map(
      "height" -> ESValue("height", ESLong)(height),
      "lastBlockTimestamp" -> ESValue("lastBlockTimestamp", ESLong)(lastBlockTimestamp),
      "stateDigest" -> ESValue("stateDigest", ESByteVector)(stateDigest)
    )
    ESObject(ESState.identifier, fields)
  }
}
