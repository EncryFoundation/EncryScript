package encrywm.backend.executor.env

import encrywm.backend.env.{ESObject, ESValue}
import encrywm.lib.Types._
import encrywm.lib.predef.env._
import scorex.utils.Random

class ESContextBuilder(stateD: ESStateData,
                       transactionD: ESTransactionData) extends ESEnvConvertable {

  val instanceName: String = "context"

  override val esType: ESProduct = ESContext

  override def asVal: ESValue = ESValue(instanceName, ESContext)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "transaction" -> ESValue("transaction", ESTransaction)(new ESTransactionBuilder(transactionD).convert),
      "state" -> ESValue("state", ESState)(new ESStateBuilder(stateD).convert),
      //Fake fields
      "proof" -> ESValue("proof", ESProof)(1),
      "self" -> ESValue("self", ESScript)(Random.randomBytes())
    )
    ESObject(ESContext.ident, fields, esType)
  }
}
