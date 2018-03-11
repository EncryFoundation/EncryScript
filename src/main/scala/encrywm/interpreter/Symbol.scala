package encrywm.interpreter

trait Symbol {

  val name: String
  val tpe: Option[BuiltInTypeSymbol] = None
}

case class BuiltInTypeSymbol(override val name: String) extends Symbol

case class BuiltInFuncSymbol(override val name: String,
                             override val tpe: Option[BuiltInTypeSymbol]) extends Symbol

case class VariableSymbol(override val name: String,
                          override val tpe: Option[BuiltInTypeSymbol]) extends Symbol
