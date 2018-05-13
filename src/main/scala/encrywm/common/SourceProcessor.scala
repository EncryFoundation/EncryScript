package encrywm.common

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.AstCodec._
import encrywm.tl.SchemaConverter
import encrywm.frontend.parser.{Lexer, Parser}
import encrywm.frontend.semantics.{ComplexityAnalyzer, StaticAnalyser, Transformer}
import encrywm.lib.TypeSystem
import scorex.crypto.hash.Blake2b256

import scala.util.Try

object SourceProcessor {

  type SerializedContract = Array[Byte]

  case object SchemaError extends Error("Invalid schema")

  /**
    * Performs source code processing according to the following algorithm:
    * 1. Split the source into TL Schema part and script part
    * 2. Process TL Schema source
    * 3. Process script source
    */
  def process(s: String): Try[Contract] = Try {
    val comps = s.split(Lexer.SchemaSeparator)
    (if (comps.size > 1) {
      // TODO: Pipeline schema error properly.
      val schemas = encrytl.common.SourceProcessor.process(comps.head)
        .getOrElse(throw SchemaError)
      val parsedScript = Parser.parse(comps.last).get.value
      new StaticAnalyser(
        TypeSystem(schemas.map(s => SchemaConverter.schema2ESType(s)
          .getOrElse(throw SchemaError)))
      ).analyse(parsedScript) match {
        case Right(_) => Transformer.scan(parsedScript)
        case Left(StaticAnalyser.StaticAnalysisFailure(r)) => throw new Error(r)
      }
    } else {
      val parsed = Parser.parse(s).get.value
      val analyzer = new StaticAnalyser(TypeSystem.default)
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
