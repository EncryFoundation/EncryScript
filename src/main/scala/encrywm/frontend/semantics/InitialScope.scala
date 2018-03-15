package encrywm.frontend.semantics

import encrywm.builtins.{Builtins, Transaction}

// TODO: Implement Scope initialization process.
object InitialScope {

  val tx = Transaction(1234567888, "Ivan")

  private val builtinSymbs = Builtins.builtinTypes.map(t => BuiltInTypeSymbol(t)) :+
    BuiltInTypeSymbol(tx.name, tx.attrs.map(attr => VariableSymbol(attr.name, Some(BuiltInTypeSymbol(attr.tpe.name)))))

  def global: ScopedSymbolTable = {
    val symtab = new ScopedSymbolTable("GLOBAL", 1)
    builtinSymbs.foreach(symtab.insert)
    symtab
  }
}
