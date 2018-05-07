package utils

import encrywm.ast.Ast.AST_NODE
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.{StaticAnalyser, Transformer}
import encrywm.lib.TypeSystem
import fastparse.all._

trait SourceProcessor {

  def precess(s: String): AST_NODE = {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    val analyser = new StaticAnalyser(TypeSystem.empty)
    analyser.scan(parsed)
    Transformer.scan(parsed)
  }
}
