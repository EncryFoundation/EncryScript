package encrywm.frontend.semantics

trait Symbol {

  val name: String
  val tpeOpt: Option[BuiltInTypeSymbol] = None
}

case class BuiltInTypeSymbol(override val name: String) extends Symbol

case class FuncSymbol(override val name: String,
                      override val tpeOpt: Option[BuiltInTypeSymbol]) extends Symbol

case class VariableSymbol(override val name: String,
                          override val tpeOpt: Option[BuiltInTypeSymbol]) extends Symbol
