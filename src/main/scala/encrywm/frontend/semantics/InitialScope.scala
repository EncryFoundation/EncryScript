package encrywm.frontend.semantics

import encrywm.builtins.Builtins

object InitialScope {

  private val builtinSymbs = Builtins.builtinTypes.map(t => BuiltInTypeSymbol(t))

  def global: ScopedSymbolTable = {
    val symtab = new ScopedSymbolTable("GLOBAL", 1)
    builtinSymbs.foreach(symtab.insert)
    symtab
  }
}
