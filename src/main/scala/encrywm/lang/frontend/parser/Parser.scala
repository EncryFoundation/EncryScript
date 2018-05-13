package encrywm.lang.frontend.parser

import encrywm.ast.Ast
import fastparse.core
import fastparse.all._

object Parser {

  def parse(source: String): core.Parsed[Ast.TREE_ROOT.Contract, Char, String] = ( Statements.contract ~ End ).parse(source)
}
