package encrywm.backend.executor.env

import encrywm.backend.env.ESEnvComponent
import encrywm.lib.predef.functions.{Blake2b256Hash, CheckSig}

class ESPredefTestEnv(contextBuilder: ESContextBuilder) {

  val predefMembers: Map[String, ESEnvComponent] = Map(
    CheckSig.name -> CheckSig.asFunc,
    Blake2b256Hash.name -> Blake2b256Hash.asFunc,
    contextBuilder.instanceName -> contextBuilder.asVal,
  )
}
