package encrywm.lib.predef

import encrywm.lib.predef.functions.hash._
import encrywm.lib.predef.functions.signature.CheckSig

package object functions {

  val hashFunctions: Seq[BuiltInFunctionHolder] = Seq(
    Blake2b256Hash,
    Blake2b512Hash,
    Keccak256Hash,
    Keccak512Hash,
    Sha256Hash
  )

  val heavyFunctions: Seq[BuiltInFunctionHolder] = Seq(
    CheckSig
  )
}
