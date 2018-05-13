package encrywm.lang.backend.executor.env

import encrywm.lang.backend.env.{ESEnvConvertable, ESObject, ESValue}
import encrywm.lib.Types.{ESTransaction, _}

case class ESTransactionData(accountPubKey: Array[Byte],
                             signature: Array[Byte],
                             bodyBytes: Array[Byte],
                             timestamp: Long)

class ESTransactionBuilder(d: ESTransactionData) extends ESEnvConvertable {

  val instanceName: String = "transaction"

  override val esType: ESProduct = ESTransaction

  override def asVal: ESValue = ESValue(instanceName, ESTransaction)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "accountPubKey" -> ESValue("accountPubKey", ESByteVector)(d.accountPubKey),
      "signature" -> ESValue("signature", ESByteVector)(d.signature),
      "bodyBytes" -> ESValue("bodyBytes", ESByteVector)(d.bodyBytes),
      "timestamp" -> ESValue("timestamp", ESLong)(d.timestamp)
    )
    ESObject(ESTransaction.ident, fields, esType)
  }
}
