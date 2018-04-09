package encrywm.lib.predef

import encrywm.backend.env.ESEnvComponent
import encrywm.lib.predef.functions.hash.ESPredefHash
import encrywm.lib.predef.functions.signature.ESPredefSignature
import encrywm.lib.predef.functions.time.ESPredefTime

object ESPredefEnv {

  val predefFunctions: Map[String, ESEnvComponent] =
    ESPredefTime.predefFunctions ++
    ESPredefHash.predefFunctions ++
    ESPredefSignature.predefFunctions
}
