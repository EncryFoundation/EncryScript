package encrywm.common

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.AstCodec._
import encrywm.frontend.parser.{Lexer, Parser}
import encrywm.frontend.semantics.{ComplexityAnalyzer, StaticAnalyser, Transformer}
import encrywm.lib.TypeSystem
import scorex.crypto.hash.Blake2b256

import scala.util.Try

object SourceProcessor {

  type SerializedContract = Array[Byte]

  def process(s: String): Try[Contract] = Try {
    val comps = s.split(Lexer.SchemaSeparator)
    (if (comps.size > 1) {
      val parsed = Parser.parse(comps.last).get.value
      // TODO: Process schema.
      Transformer.scan(parsed)
    } else {
      val parsed = Parser.parse(s).get.value
      val analyzer = new StaticAnalyser(TypeSystem.empty)
      analyzer.scan(parsed)
      Transformer.scan(parsed)
    }).asInstanceOf[Contract]
  }

  def source2Contract(s: String): Try[EncryContract] = process(s).map { c =>
    val complexityScore = ComplexityAnalyzer.scan(c)
    val serializedScript = ScriptSerializer.serialize(c)
    val fingerprint = getScriptFingerprint(serializedScript)
    EncryContract(serializedScript, ScriptMeta(complexityScore, fingerprint))
  }

  def source2SerializedContract(s: String): Try[SerializedContract] = process(s).map { p =>
    codec.encode(p).require.toByteArray
  }

  def getScriptFingerprint(ss: SerializedScript): ScriptFingerprint = Blake2b256.hash(ss).take(8)
}
