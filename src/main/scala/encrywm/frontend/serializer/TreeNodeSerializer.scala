package encrywm.frontend.serializer

import encrywm.frontend.parser.Ast

trait TreeNodeSerializer[NT <: Ast.AST_NODE] {

  def toBytes(tree: Ast.AST_NODE): Array[Byte]

  def fromBytes(bytes: Array[Byte]): Ast.AST_NODE
}
