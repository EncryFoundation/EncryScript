package encrywm.builtins.environment.context

import encrywm.backend.executor.context.{ESObject, ESValue}
import encrywm.builtins.Types._
import encrywm.builtins.Types.ESTransaction

case class ESTransactionBuilder(accountPubKey: Array[Byte],
                                signature: Array[Byte],
                                bodyBytes: Array[Byte],
                                timestamp: Long) {

  def nativeObject: ESObject = {
    val fields = Map(
      "accountPubKey" -> ESValue("accountPubKey", ESByteVector)(accountPubKey),
      "signature" -> ESValue("signature", ESByteVector)(signature),
      "bodyBytes" -> ESValue("bodyBytes", ESByteVector)(bodyBytes),
      "timestamp" -> ESValue("timestamp", ESLong)(timestamp),
    )
    ESObject(ESTransaction.identifier, fields)
  }
}
