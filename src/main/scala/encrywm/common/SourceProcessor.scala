package encrywm.common

import encrywm.ast.AstCodec._
import encrywm.lang.ESCompiler
import encrywm.lang.frontend.semantics.ComplexityAnalyzer
import scorex.crypto.hash.Blake2b256

import scala.util.Try

object SourceProcessor {

  type SerializedContract = Array[Byte]

  def source2Contract(s: String): Try[EncryContract] = ESCompiler.compile(s).map { c =>
    val complexityScore = ComplexityAnalyzer.complexityOf(c)
    val serializedScript = ScriptSerializer.serialize(c)
    val fingerprint = getScriptFingerprint(serializedScript)
    EncryContract(serializedScript, ScriptMeta(complexityScore, fingerprint))
  }

  def source2SerializedContract(s: String): Try[SerializedContract] = ESCompiler.compile(s).map { p =>
    codec.encode(p).require.toByteArray
  }

  def getScriptFingerprint(ss: SerializedScript): ScriptFingerprint = Blake2b256.hash(ss).take(8)
}