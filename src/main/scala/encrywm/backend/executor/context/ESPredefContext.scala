package encrywm.backend.executor.context

import encrywm.core.environment.context.{ESStateBuilder, ESTransactionBuilder}
import encrywm.core.environment.functions.CheckSig

class ESPredefContext(transaction: ESTransactionBuilder, state: ESStateBuilder) {

  val predefMembers: Map[String, ESRuntimeComponent] = Map(
    CheckSig.name -> CheckSig.asFunc,
    transaction.instanceName -> transaction.asVal,
    state.instanceName -> state.asVal
  )
}
