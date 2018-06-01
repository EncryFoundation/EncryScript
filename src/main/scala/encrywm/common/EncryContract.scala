package encrywm.common

import encrywm.lang.backend.env.{ESEnvConvertable, ESObject, ESValue}
import encrywm.lang.frontend.semantics.ComplexityAnalyzer
import encrywm.lang.frontend.semantics.ComplexityAnalyzer.ScriptComplexityScore
import encrywm.lib.Types.{ESByteVector, ESProduct, ESScript}

case class ScriptMeta(complexityScore: ScriptComplexityScore, scriptFingerprint: ScriptFingerprint) extends ESEnvConvertable {

  override val esType: ESProduct = ESScript

  override def asVal: ESValue = ESValue(ESScript.ident.toLowerCase, ESScript)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "fingerprint" -> ESValue("stateDigest", ESByteVector)(scriptFingerprint)
    )
    ESObject(ESScript.ident, fields, esType)
  }
}

case class EncryContract(serializedScript: SerializedScript, meta: ScriptMeta) {

  def validMeta: Boolean = {
    (SourceProcessor.getScriptFingerprint(serializedScript) sameElements meta.scriptFingerprint) &&
    ScriptSerializer.deserialize(serializedScript).map { s =>
      val complexity = ComplexityAnalyzer.complexityOfScript(s)
      complexity == meta.complexityScore
    }.getOrElse(false)
  }
}
