package encrywm.frontend.semantics.scope

import encrywm.builtins.{Attribute, ComplexType}

// TODO: Implement Scope initialization process.
object InitialScope {

  import encrywm.builtins.Types._

  val attrs = Seq(
    Attribute("timestamp", LONG),
    Attribute("sender", STRING)
  )
  val tx = ComplexType("transaction", attrs)

  private val builtinSymbs = staticTypes.map(_._2.symbol).toSeq :+
    BuiltInTypeSymbol(tx.name, tx.attrs.map(attr => VariableSymbol(attr.name, Some(BuiltInTypeSymbol(attr.tpe.identifier)))))

  def global: ScopedSymbolTable = {
    val symtab = new ScopedSymbolTable("GLOBAL", 1)
    builtinSymbs.foreach(symtab.insert)
    symtab
  }
}
