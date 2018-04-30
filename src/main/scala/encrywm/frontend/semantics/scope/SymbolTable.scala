package encrywm.frontend.semantics.scope

import encrywm.ast.Ast.AST_NODE
import encrywm.ast.AstStringifier
import encrywm.frontend.semantics.error.AlreadyDefinedError

import scala.collection.mutable

trait SymbolTable {

  protected val symbols: mutable.TreeMap[String, Symbol] = mutable.TreeMap.empty[String, Symbol]

  def insert(sym: Symbol, node: AST_NODE): Unit = {
    symbols.get(sym.name).map(_ => throw AlreadyDefinedError(sym.name, AstStringifier.toString(node)))
    symbols.update(sym.name, sym)
  }

  def lookup(name: String, currentScopeOnly: Boolean = false): Option[Symbol] = symbols.get(name)
}
