package encrywm.common

import encrywm.frontend.semantics.ComplexityAnalyzer
import encrywm.frontend.semantics.ComplexityAnalyzer.ScriptComplexityScore

case class ScriptMeta(complexityScore: ScriptComplexityScore, scriptFingerprint: ScriptFingerprint)

case class EncryContract(serializedScript: SerializedScript, meta: ScriptMeta) {

  def validMeta: Boolean = {
    (SourceProcessor.getScriptFingerprint(serializedScript) sameElements meta.scriptFingerprint) &&
    ScriptSerializer.deserialize(serializedScript).map { s =>
      val complexity = ComplexityAnalyzer.scan(s)
      complexity == meta.complexityScore
    }.getOrElse(false)
  }
}
