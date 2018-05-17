package encrywm.lang.frontend.semantics.scope

import encrywm.ast.Ast.EXPR.Name
import encrywm.ast.Ast.EXPR_CTX.Load
import encrywm.ast.Ast.Identifier

class ScopedSymbolTable(val scopeName: String,
                        val scopeLevel: Int,
                        val parentalScopeOpt: Option[ScopedSymbolTable] = None) extends SymbolTable {

  override def lookup(name: String, currentScopeOnly: Boolean = false): Option[Symbol] = symbols.get(name) match {
      case Some(r) => Some(r)
      case None if !currentScopeOnly => parentalScopeOpt.flatMap(_.lookup(name))
      case _ => None
    }

  override def toString: String = s"L$scopeLevel ${this.symbols}"
}

object ScopedSymbolTable {

  def apply(name: String, oldScope: ScopedSymbolTable): ScopedSymbolTable =
    new ScopedSymbolTable(name, oldScope.scopeLevel + 1, Some(oldScope))

  def initialized: ScopedSymbolTable = {
    val symbolTable = new ScopedSymbolTable("GLOBAL", 1)
    ESPredefScope.predefNames.foreach(sym => symbolTable.insert(sym, Name(Identifier(sym.name), Load)))
    symbolTable
  }
}
