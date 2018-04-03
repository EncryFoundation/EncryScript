package encrywm.core.predef

import encrywm.backend.env.ESEnvComponent
import encrywm.core.predef.functions.{Blake2b256Hash, CheckSig}

object ESPredefEnv {

  val predefFunctions: Map[String, ESEnvComponent] = Map(
    CheckSig.name -> CheckSig.asFunc,
    Blake2b256Hash.name -> Blake2b256Hash.asFunc
  )
}
