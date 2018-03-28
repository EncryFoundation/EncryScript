package encrywm.backend.env

import encrywm.core.environment.context.ESContextBuilder
import encrywm.core.environment.functions.CheckSig

class ESPredefEnv(contextBuilder: ESContextBuilder) {

  val predefMembers: Map[String, ESEnvComponent] = Map(
    CheckSig.name -> CheckSig.asFunc,
    contextBuilder.instanceName -> contextBuilder.asVal,
  )
}
