package encrywm.lib.predef.functions

import encrywm.lang.backend.env.ESEnvComponent

package object time {

  val predefFunctions: Map[String, ESEnvComponent] = Map(
    Str2Time.name -> Str2Time.asFunc
  )
}
