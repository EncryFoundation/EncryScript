package encrywm.common

import encrywm.frontend.semantics.ComplexityAnalyzer
import encrywm.frontend.semantics.ComplexityAnalyzer.ScriptComplexityScore

case class ScriptMeta(complexityScore: ScriptComplexityScore)

case class ESContract(serializedScript: SerializedScript, meta: ScriptMeta) {

  def validMeta: Boolean = {
    ScriptSerializer.deserialize(serializedScript).map { s =>
      val complexity = ComplexityAnalyzer.scan(s)
      complexity == meta.complexityScore
    }.getOrElse(false)
  }
}
