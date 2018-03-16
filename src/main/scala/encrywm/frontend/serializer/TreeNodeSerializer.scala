package encrywm.frontend.serializer

import encrywm.frontend.parser.Ast

trait TreeNodeSerializer[NT <: Ast.AST_NODE] {

  val prefix: Byte

  def toBytes(tree: NT): Array[Byte]

  def fromBytes(bytes: Array[Byte]): Option[NT]
}
