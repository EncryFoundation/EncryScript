package encrywm.lib.predef.functions.hash

import encrywm.backend.env.ESEnvComponent
import encrywm.lib.predef.functions.signature.CheckSig

object ESPredefHash {

  val predefFunctions: Map[String, ESEnvComponent] = Map(
    Blake2b256Hash.name -> Blake2b256Hash.asFunc,
    Blake2b512Hash.name -> Blake2b512Hash.asFunc,
    Keccak256Hash.name -> Keccak256Hash.asFunc,
    Keccak512Hash.name -> Keccak512Hash.asFunc,
    Sha256Hash.name -> Sha256Hash.asFunc
  )
}
