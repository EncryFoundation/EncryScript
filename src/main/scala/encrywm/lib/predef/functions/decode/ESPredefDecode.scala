package encrywm.lib.predef.functions.decode

import encrywm.lang.backend.env.ESEnvComponent

object ESPredefDecode {
  val predefFunctions: Map[String, ESEnvComponent] = Map(
    Base58decode.name -> Base58decode.asFunc
  )
}
