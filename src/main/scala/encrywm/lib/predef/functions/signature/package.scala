package encrywm.lib.predef.functions

import encrywm.lang.backend.env.ESEnvComponent

package object signature {

  val predefFunctions: Map[String, ESEnvComponent] = Map(
    CheckSig.name -> CheckSig.asFunc
  )
}
