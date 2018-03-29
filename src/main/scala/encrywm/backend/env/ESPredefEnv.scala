package encrywm.backend.env

import encrywm.core.predef.context.ESContextBuilder
import encrywm.core.predef.functions.{Blake2b256Hash, CheckSig}

class ESPredefEnv(contextBuilder: ESContextBuilder) {

  val predefMembers: Map[String, ESEnvComponent] = Map(
    CheckSig.name -> CheckSig.asFunc,
    Blake2b256Hash.name -> Blake2b256Hash.asFunc,
    contextBuilder.instanceName -> contextBuilder.asVal,
  )
}
