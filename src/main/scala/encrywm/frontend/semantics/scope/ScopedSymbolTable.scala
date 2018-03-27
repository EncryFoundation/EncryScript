package encrywm.frontend.semantics.scope

import encrywm.builtins.Types.{ESBoolean, ESTransaction}

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

  val predefNames: Seq[Symbol] = Seq(
    ValSymbol("transaction", ESTransaction),
    FuncSymbol("checkSig", ESBoolean)
  )

  def apply(name: String, oldScope: ScopedSymbolTable): ScopedSymbolTable =
    new ScopedSymbolTable(name, oldScope.scopeLevel + 1, Some(oldScope))

  def initialized: ScopedSymbolTable = {
    val symbolTable = new ScopedSymbolTable("GLOBAL", 1)
    predefNames.foreach(symbolTable.insert)
    symbolTable
  }
}
