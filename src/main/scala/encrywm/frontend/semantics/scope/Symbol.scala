package encrywm.frontend.semantics.scope

import encrywm.lib.Types.ESType

sealed trait Symbol {
  val name: String
  val tpe: ESType
}

case class FuncSymbol(override val name: String,
                      override val tpe: ESType,
                      params: Seq[ValSymbol] = Seq()) extends Symbol

case class ValSymbol(override val name: String,
                     override val tpe: ESType) extends Symbol
