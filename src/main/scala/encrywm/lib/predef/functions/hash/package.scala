package encrywm.lib.predef.functions

import encrywm.lang.backend.env.ESEnvComponent

package object hash {

  val predefFunctions: Map[String, ESEnvComponent] = Map(
    Blake2b256Hash.name -> Blake2b256Hash.asFunc,
    Blake2b512Hash.name -> Blake2b512Hash.asFunc,
    Keccak256Hash.name -> Keccak256Hash.asFunc,
    Keccak512Hash.name -> Keccak512Hash.asFunc,
    Sha256Hash.name -> Sha256Hash.asFunc
  )
}
