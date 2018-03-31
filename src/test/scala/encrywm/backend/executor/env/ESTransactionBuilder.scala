package encrywm.backend.executor.env

import encrywm.backend.env.{ESObject, ESValue}
import encrywm.core.Types.{ESTransaction, _}
import encrywm.core.predef.env.ESEnvConvertable

case class ESTransactionData(accountPubKey: Array[Byte],
                             signature: Array[Byte],
                             bodyBytes: Array[Byte],
                             timestamp: Long)

class ESTransactionBuilder(d: ESTransactionData) extends ESEnvConvertable {

  val instanceName: String = "transaction"

  override def asVal: ESValue = ESValue(instanceName, ESTransaction)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "accountPubKey" -> ESValue("accountPubKey", ESByteVector)(d.accountPubKey),
      "signature" -> ESValue("signature", ESByteVector)(d.signature),
      "bodyBytes" -> ESValue("bodyBytes", ESByteVector)(d.bodyBytes),
      "timestamp" -> ESValue("timestamp", ESLong)(d.timestamp)
    )
    ESObject(ESTransaction.ident, fields)
  }
}
