package encrywm.lib.predef

import encrywm.lib.predef.functions.decode.SchemaDecode
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

  /** Computationally expensive functions */
  val middleFunctions: Seq[BuiltInFunctionHolder] = Seq(
    SchemaDecode
  )

  /** The most computationally expensive functions */
  val heavyFunctions: Seq[BuiltInFunctionHolder] = Seq(
    CheckSig
  )
}
