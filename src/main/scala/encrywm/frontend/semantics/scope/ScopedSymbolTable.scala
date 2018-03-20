package encrywm.frontend.semantics.scope

class ScopedSymbolTable(val scopeName: String,
                        val scopeLevel: Int,
                        val parentalScopeOpt: Option[ScopedSymbolTable] = None) extends SymbolTable {

  override def lookup(name: String, currentScopeOnly: Boolean = false): Option[Symbol] = symbols.get(name) match {
      case Some(r) => Some(r)
      case None if !currentScopeOnly => parentalScopeOpt.flatMap(_.lookup(name))
      case _ => None
    }
}

object ScopedSymbolTable {

  def apply(name: String, oldScope: ScopedSymbolTable): ScopedSymbolTable =
    new ScopedSymbolTable(name, oldScope.scopeLevel + 1, Some(oldScope))
}
