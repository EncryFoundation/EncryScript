package encrywm.lib.predef.functions.signature

import encrywm.backend.env.ESEnvComponent

object ESPredefSignature {
  val predefFunctions: Map[String, ESEnvComponent] = Map(
    CheckSig.name -> CheckSig.asFunc
  )
}
