package encrywm.ast

trait AstNodeScanner {

  def scan(node: Ast.AST_NODE): Any
}
