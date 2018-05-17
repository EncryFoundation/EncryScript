package encrywm.lib.predef.functions

import encrywm.lang.backend.env.ESEnvComponent

package object decode {

  val predefFunctions: Map[String, ESEnvComponent] = Map(
    Base58decode.name -> Base58decode.asFunc,
    SchemaDecode.name -> SchemaDecode.asFunc
  )
}
