package encrywm.lib.predef.functions.time

import encrywm.backend.env.ESEnvComponent

object ESPredefTime {
  val predefFunctions: Map[String, ESEnvComponent] = Map(
    FromStr2Timestamp.name -> FromStr2Timestamp.asFunc
  )
}
