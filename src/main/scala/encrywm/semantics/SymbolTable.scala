package encrywm.semantics

import scala.collection.mutable

class SymbolTable {

  private val symbols = mutable.TreeMap.empty[String, Symbol]

  def define(sym: Symbol): Unit = symbols.update(sym.name, sym)

  def lookup(key: String): Option[Symbol] = symbols.get(key)
}
