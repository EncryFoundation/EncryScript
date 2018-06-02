package encrywm.lang.frontend.semantics.scope

import encrywm.ast.Ast.EXPR.Name
import encrywm.ast.Ast.Identifier

case class ScopedSymbolTable(scopeName: String,
                             scopeLevel: Int,
                             parentalScopeOpt: Option[ScopedSymbolTable] = None,
                             isFunc: Boolean = false) extends SymbolTable {

  override def lookup(name: String, currentScopeOnly: Boolean = false): Option[Symbol] = symbols.get(name) match {
    case Some(r) => Some(r)
    case None if !currentScopeOnly => parentalScopeOpt.flatMap(_.lookup(name))
    case _ => None
  }

  override def toString: String = s"L$scopeLevel ${this.symbols}"
}

object ScopedSymbolTable {

  def nested(name: String, oldScope: ScopedSymbolTable, isFunc: Boolean = false): ScopedSymbolTable =
    new ScopedSymbolTable(name, oldScope.scopeLevel + 1, Some(oldScope), isFunc)

  def initialized: ScopedSymbolTable = {
    val symbolTable = new ScopedSymbolTable("GLOBAL", 1)
    ESPredefScope.predefNames.foreach(sym => symbolTable.insert(sym, Name(Identifier(sym.name))))
    symbolTable
  }
}
