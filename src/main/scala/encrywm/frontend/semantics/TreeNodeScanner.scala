package encrywm.frontend.semantics

import encrywm.frontend.ast.Ast

trait TreeNodeScanner {

  def scan(node: Ast.AST_NODE): Unit
}
