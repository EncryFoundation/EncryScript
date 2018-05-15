package encrywm.common

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.AstCodec._
import encrywm.lang.frontend.parser.{Lexer, Parser}
import encrywm.lang.frontend.semantics.{ComplexityAnalyzer, Optimizer, SchemaBinder, StaticProcessor, Transformer}
import encrywm.lib.TypeSystem
import encrywm.typelang.SchemaConverter
import scorex.crypto.hash.Blake2b256

import scala.util.Try

object SourceProcessor {

  type SerializedContract = Array[Byte]

  /**
    * Performs source code processing according to the following algorithm:
    * 1. Split the source into TL Schema part and script part
    * 2. Interpret schema source
    * 3. Parse script source
    * 4. Initialize `StaticProcessor` with schemas (from step 2)
    * 5. Process script
    * 6. Finally, perform Optimization -> Schema Types Binding -> Transformation
    */
  def process(s: String): Try[Contract] = {
    val comps = s.split(Lexer.SchemaSeparator)
    if (comps.length > 1) {
      encrytl.common.SourceProcessor.process(comps.head).flatMap { schemas =>
        Parser.parse(comps.last).flatMap { parsedScript =>
          new StaticProcessor(
            TypeSystem(schemas.map(s => SchemaConverter.schema2ESType(s)
              .getOrElse(throw new Exception("Schema conversion failed"))
            ))
          ).process(parsedScript).map { res =>
            Transformer.transform(SchemaBinder.bind(new Optimizer().optimize(res), schemas)) match {
              case c: Contract => c
              case other => throw new Exception(s"Unexpected node type: $other")
            }
          }
        }
      }
    } else {
      Parser.parse(s).flatMap { parsedScript =>
        StaticProcessor.default.process(parsedScript).map { res =>
          Transformer.transform(new Optimizer().optimize(res)) match {
            case c: Contract => c
            case other => throw new Exception(s"Unexpected node type: $other")
          }
        }
      }
    }
  }

  def source2Contract(s: String): Try[EncryContract] = process(s).map { c =>
    val complexityScore = ComplexityAnalyzer.complexityOf(c)
    val serializedScript = ScriptSerializer.serialize(c)
    val fingerprint = getScriptFingerprint(serializedScript)
    EncryContract(serializedScript, ScriptMeta(complexityScore, fingerprint))
  }

  def source2SerializedContract(s: String): Try[SerializedContract] = process(s).map { p =>
    codec.encode(p).require.toByteArray
  }

  def getScriptFingerprint(ss: SerializedScript): ScriptFingerprint = Blake2b256.hash(ss).take(8)
}