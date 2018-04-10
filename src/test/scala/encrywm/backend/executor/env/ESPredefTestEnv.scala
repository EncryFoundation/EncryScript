package encrywm.backend.executor.env

import encrywm.backend.env.ESEnvComponent
import encrywm.lib.predef.functions.hash._
import encrywm.lib.predef.functions.signature._
import encrywm.lib.predef.functions.time._

class ESPredefTestEnv(contextBuilder: ESContextBuilder) {

  val predefMembers: Map[String, ESEnvComponent] = Map(
    CheckSig.name -> CheckSig.asFunc,
    Blake2b256Hash.name -> Blake2b256Hash.asFunc,
    Blake2b512Hash.name -> Blake2b512Hash.asFunc,
    Keccak256Hash.name -> Blake2b256Hash.asFunc,
    Keccak512Hash.name -> Keccak512Hash.asFunc,
    Sha256Hash.name -> Sha256Hash.asFunc,
    Str2Time.name -> Str2Time.asFunc,
    contextBuilder.instanceName -> contextBuilder.asVal,
  )
}
