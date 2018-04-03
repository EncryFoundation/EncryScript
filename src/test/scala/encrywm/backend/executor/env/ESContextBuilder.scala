package encrywm.backend.executor.env

import encrywm.backend.env.{ESObject, ESValue}
import encrywm.core.Types._
import encrywm.core.predef.env._

class ESContextBuilder(stateD: ESStateData,
                       transactionD: ESTransactionData) extends ESEnvConvertable {

  val instanceName: String = "context"

  override val esType: ESProduct = ESContext

  override def asVal: ESValue = ESValue(instanceName, ESContext)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "transaction" -> ESValue("transaction", ESTransaction)(new ESTransactionBuilder(transactionD).convert),
      "state" -> ESValue("state", ESState)(new ESStateBuilder(stateD).convert),
    )
    ESObject(ESContext.ident, fields, esType)
  }
}
