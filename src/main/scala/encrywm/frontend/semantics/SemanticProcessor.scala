package encrywm.frontend.semantics

import encrywm.frontend.parser.Ast

import scala.util.Try

object SemanticProcessor {

  def processTree(treeRoot: Ast.TREE_ROOT): Try[Ast.TREE_ROOT] = Try {
    val scope = StaticScanner.scanTreeAndGetSymtable(treeRoot)
    val typeScanner = new TypeScanner(treeRoot, scope)
    typeScanner.processedTree
  }
}
