package encrywm.core.environment.context
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.core.Types._

class ESContextBuilder(stateD: ESStateData,
                       transactionD: ESTransactionData) extends EnvComponentBuilder {

  val instanceName: String = "context"

  override def asVal: ESValue = ESValue(instanceName, ESContext)(build)

  override def build: ESObject = {
    val fields = Map(
      "transaction" -> ESValue("transaction", ESTransaction)(new ESTransactionBuilder(transactionD).build),
      "state" -> ESValue("state", ESState)(new ESStateBuilder(stateD).build),
    )
    ESObject(ESContext.identifier, fields)
  }
}
