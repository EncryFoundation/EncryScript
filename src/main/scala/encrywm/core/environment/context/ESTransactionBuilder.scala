package encrywm.core.environment.context

import encrywm.backend.env.{ESObject, ESValue}
import encrywm.core.Types._
import encrywm.core.Types.ESTransaction

case class ESTransactionData(accountPubKey: Array[Byte],
                             signature: Array[Byte],
                             bodyBytes: Array[Byte],
                             timestamp: Long)

class ESTransactionBuilder(d: ESTransactionData) extends EnvComponentBuilder {

  val instanceName: String = "transaction"

  override def asVal: ESValue = ESValue(instanceName, ESTransaction)(build)

  override def build: ESObject = {
    val fields = Map(
      "accountPubKey" -> ESValue("accountPubKey", ESByteVector)(d.accountPubKey),
      "signature" -> ESValue("signature", ESByteVector)(d.signature),
      "bodyBytes" -> ESValue("bodyBytes", ESByteVector)(d.bodyBytes),
      "timestamp" -> ESValue("timestamp", ESLong)(d.timestamp)
    )
    ESObject(ESTransaction.identifier, fields)
  }
}
