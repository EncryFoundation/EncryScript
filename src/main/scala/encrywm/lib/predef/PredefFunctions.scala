package encrywm.lib.predef

import encrywm.lang.backend.env.ESEnvComponent
import encrywm.lib.predef.decode.{Base58decode, SchemaDecode}
import encrywm.lib.predef.hash._
import encrywm.lib.predef.signature.CheckSig
import encrywm.lib.predef.time.Str2Time

object PredefFunctions {

  /** All predef functions by their categories. */
  val timeF: Seq[BuiltInFunctionHolder] = Seq(Str2Time)
  val cryptoF: Seq[BuiltInFunctionHolder] = Seq(CheckSig)
  val hashF: Seq[BuiltInFunctionHolder] = Seq(
    Blake2b256Hash,
    Blake2b256Hash,
    Keccak256Hash,
    Keccak512Hash,
    Sha256Hash
  )
  val decodeF: Seq[BuiltInFunctionHolder] = Seq(
    Base58decode,
    SchemaDecode
  )

  val all: Map[String, ESEnvComponent] =
    (timeF ++ cryptoF ++ hashF ++ decodeF).map(f => f.name -> f.asFunc).toMap

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
