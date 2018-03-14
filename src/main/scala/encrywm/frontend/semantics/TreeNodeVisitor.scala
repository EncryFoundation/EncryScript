package encrywm.frontend.semantics

import encrywm.frontend.parser.Ast

trait TreeNodeVisitor {

  def visit(node: Ast.AST_NODE): Unit
}
