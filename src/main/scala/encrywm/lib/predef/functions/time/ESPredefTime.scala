package encrywm.lib.predef.functions.time

import encrywm.backend.env.ESEnvComponent

object ESPredefTime {
  val predefFunctions: Map[String, ESEnvComponent] = Map(
    Str2Time.name -> Str2Time.asFunc
  )
}
