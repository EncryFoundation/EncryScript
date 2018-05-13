package encrywm.lib

import encrywm.lang.backend.env.ESEnvComponent
import encrywm.lib.predef.functions.decode.ESPredefDecode
import encrywm.lib.predef.functions.hash.ESPredefHash
import encrywm.lib.predef.functions.signature.ESPredefSignature
import encrywm.lib.predef.functions.time.ESPredefTime

package object predef {

  val predefFunctions: Map[String, ESEnvComponent] =
    ESPredefTime.predefFunctions ++
      ESPredefHash.predefFunctions ++
      ESPredefSignature.predefFunctions ++
      ESPredefDecode.predefFunctions
}
