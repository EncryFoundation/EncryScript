package encrywm.frontend.serializer

import encrywm.frontend.parser.Ast

object AstSerializer {

  def toBytes(tree: Ast.AST_NODE): Array[Byte] = ???

  def fromBytes(bytes: Array[Byte]): Ast.AST_NODE = ???
}
