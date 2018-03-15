package encrywm.frontend.semantics

import encrywm.builtins.{Attribute, Builtins, ESObject}
import encrywm.frontend.parser.Ast

// TODO: Implement Scope initialization process.
object InitialScope {

  val attrs = Set(
    Attribute("timestamp", Ast.TYPE.LONG, 1123455L),
    Attribute("sender", Ast.TYPE.STRING, "Ivan")
  )
  val tx = ESObject("transaction", attrs)

  private val builtinSymbs = Builtins.StaticBuiltInTypes.map(_.symbol).toSeq :+
    BuiltInTypeSymbol(tx.name, tx.attrs.map(attr => VariableSymbol(attr.name, Some(BuiltInTypeSymbol(attr.tpe.name)))))

  def global: ScopedSymbolTable = {
    val symtab = new ScopedSymbolTable("GLOBAL", 1)
    builtinSymbs.foreach(symtab.insert)
    symtab
  }
}
