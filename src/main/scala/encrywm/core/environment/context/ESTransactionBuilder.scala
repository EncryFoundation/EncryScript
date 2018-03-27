package encrywm.core.environment.context

import encrywm.backend.executor.context.{ESObject, ESValue}
import encrywm.core.Types._
import encrywm.core.Types.ESTransaction

case class ESTransactionBuilder(accountPubKey: Array[Byte],
                                signature: Array[Byte],
                                bodyBytes: Array[Byte],
                                timestamp: Long) {

  val instanceName: String = "transaction"

  def asVal: ESValue = ESValue(instanceName, ESTransaction)(nativeObject)

  def nativeObject: ESObject = {
    val fields = Map(
      "accountPubKey" -> ESValue("accountPubKey", ESByteVector)(accountPubKey),
      "signature" -> ESValue("signature", ESByteVector)(signature),
      "bodyBytes" -> ESValue("bodyBytes", ESByteVector)(bodyBytes),
      "timestamp" -> ESValue("timestamp", ESLong)(timestamp)
    )
    ESObject(ESTransaction.identifier, fields)
  }
}
