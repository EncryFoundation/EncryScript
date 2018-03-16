package encrywm.frontend.semantics

import encrywm.frontend.parser.Ast

trait TreeNodeScanner {

  def scan(node: Ast.AST_NODE): Unit
}
