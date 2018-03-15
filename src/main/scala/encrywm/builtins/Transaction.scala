package encrywm.builtins

import encrywm.frontend.parser.Ast

case class Transaction(timestamp: Long, sender: String) extends ESObject {

  override val name: String = "transaction"

  override val attrs: Set[Attribute] = Set(
    Attribute("timestamp", Ast.TYPE.LONG, timestamp),
    Attribute("sender", Ast.TYPE.STRING, sender)
  )
}
