package utils

import encrywm.ast.Ast.AST_NODE
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.{Transformer, StaticAnalyser}
import fastparse.all._

trait SourceProcessor {

  def precess(s: String): AST_NODE = {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    StaticAnalyser.scan(parsed)
    Transformer.scan(parsed)
  }
}
