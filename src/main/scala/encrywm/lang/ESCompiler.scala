package encrywm.lang

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.lang.frontend.parser.{Lexer, Parser}
import encrywm.lang.frontend.semantics.{Optimizer, SchemaBinder, StaticProcessor, Transformer}
import encrywm.lib.{TypeSystem, Types}
import encrywm.typelang.Converter

import scala.util.{Failure, Success, Try}

object ESCompiler {

  /**
    * Performs source code processing according to the following algorithm:
    * 1. Split the source into TL Schema part and script part
    * 2. Interpret schema source
    * 3. Parse script source
    * 4. Initialize `StaticProcessor` with schemas (from step 2)
    * 5. Process script
    * 6. Finally, perform Optimization -> Schema Types Binding -> Transformation
    */
  def compile(source: String): Try[Contract] = {
    val comps: Array[String] = source.split(Lexer.SchemaSeparator)
    if (comps.length > 1) {
      encrytl.common.SourceProcessor.process(comps.head).flatMap { schemas =>
        Parser.parse(comps.last).flatMap { parsedScript =>
          schemas.foldLeft[Try[Seq[Types.ESTypedObject]]](Success(Seq.empty[Types.ESTypedObject])) {
            case (Success(acc), schema) =>
              Converter.schema2ESType(schema) match {
                case Success(obj) => Success(acc :+ obj)
                case Failure(e) => Failure(e)
              }
            case (failure @ Failure(_), _) => failure
          }.flatMap { types =>
            StaticProcessor(TypeSystem(types)).process(parsedScript).map { res =>
              Transformer.transform(SchemaBinder.bind(new Optimizer().optimize(res), schemas)) match {
                case c: Contract => c
                case other => throw new Exception(s"Unexpected node type: $other")
              }
            }
          }
        }
      }
    } else {
      Parser.parse(source).flatMap { parsedScript =>
        StaticProcessor.default.process(parsedScript).map { res =>
          Transformer.transform(new Optimizer().optimize(res)) match {
            case c: Contract => c
            case other => throw new Exception(s"Unexpected node type: $other")
          }
        }
      }
    }
  }
}
