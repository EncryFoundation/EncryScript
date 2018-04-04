package encrywm.common

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.AstCodec._
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.{ComplexityAnalyzer, StaticAnalyser, Transformer}
import fastparse.all._
import scorex.crypto.hash.Blake2b256

import scala.util.Try

object SourceProcessor {

  type SerializedContract = Array[Byte]

  def process(s: String): Try[Contract] = Try {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    val analyzer = new StaticAnalyser
    analyzer.scan(parsed)
    val transformed = Transformer.scan(parsed)
    transformed.asInstanceOf[Contract]
  }

  def source2Contract(s: String): Try[ESContract] = process(s).map { c =>
    val complexityScore = ComplexityAnalyzer.scan(c)
    val serializedScript = ScriptSerializer.serialize(c)
    val fingerprint = getScriptFingerprint(serializedScript)
    ESContract(serializedScript, ScriptMeta(complexityScore, fingerprint))
  }

  def source2SerializedContract(s: String): Try[SerializedContract] = process(s).map { p =>
    codec.encode(p).require.toByteArray
  }

  def getScriptFingerprint(ss: SerializedScript): ScriptFingerprint = Blake2b256.hash(ss).take(8)
}
