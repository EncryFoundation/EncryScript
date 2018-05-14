package encrywm.common

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.AstCodec._
import encrywm.typelang.SchemaConverter
import encrywm.lang.frontend.parser.{Lexer, Parser}
import encrywm.lang.frontend.semantics.{ComplexityAnalyzer, Optimizer, SchemaBinder, StaticProcessor, Transformer}
import encrywm.lib.TypeSystem
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success, Try}

object SourceProcessor {

  type SerializedContract = Array[Byte]

  case object SchemaException extends Exception("Invalid schema")

  /**
    * Performs source code processing according to the following algorithm:
    * 1. Split the source into TL Schema part and script part
    * 2. Interpret schema source
    * 3. Parse script source
    * 4. Initialize `StaticProcessor` with schemas (from step 2)
    * 5. Process script
    * 6. Finally, perform Optimization -> Schema Types Binding -> Transformation
    */
  // TODO: Implement error pipelining properly.
  def process(s: String): Try[Contract] = Try {
    val comps = s.split(Lexer.SchemaSeparator)
    (if (comps.length > 1) {
      encrytl.common.SourceProcessor.process(comps.head) match {
        case Success(schemas) =>
          Parser.parse(comps.last) match {
            case Success(parsedScript) =>
              new StaticProcessor(
                TypeSystem(schemas.map(s => SchemaConverter.schema2ESType(s)
                  .getOrElse(throw SchemaException)
                ))
              ).process(parsedScript) match {
                case Right(StaticProcessor.StaticAnalysisSuccess(res)) =>
                  Transformer.transform(SchemaBinder.bind(new Optimizer().optimize(res), schemas))
                case Left(StaticProcessor.StaticAnalysisFailure(r)) => throw new Exception(r)
              }
            case Failure(e) => println(s"Exception while processing parsedScript: $e")
          }
        case Failure(e) => println(s"Exception while processing schemas: $e")
      }
    }
    else {
      val parsedScript = Parser.parse(s).get
      StaticProcessor.default.process(parsedScript) match {
        case Right(StaticProcessor.StaticAnalysisSuccess(res)) =>
          Transformer.transform(new Optimizer().optimize(res))
        case Left(StaticProcessor.StaticAnalysisFailure(r)) => throw new Error(r)
      }
    }).asInstanceOf[Contract]
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
