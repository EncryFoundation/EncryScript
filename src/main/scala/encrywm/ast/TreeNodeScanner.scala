package encrywm.ast

trait TreeNodeScanner {

  def scan(node: Ast.AST_NODE): Unit
}
