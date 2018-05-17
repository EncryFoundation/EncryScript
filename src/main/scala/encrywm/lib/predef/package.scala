package encrywm.lib

import encrywm.lang.backend.env.ESEnvComponent
import encrywm.lib.predef.functions.{decode, hash, signature, time}

package object predef {

  val predefFunctions: Map[String, ESEnvComponent] =
    time.predefFunctions ++
      hash.predefFunctions ++
      signature.predefFunctions ++
      decode.predefFunctions
}
