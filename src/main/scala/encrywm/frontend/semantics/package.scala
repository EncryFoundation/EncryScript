package encrywm.frontend

import encrywm.ast.Ast.Identifier

package object semantics {

  implicit def liftString(s: String): Identifier = Identifier(s)

  implicit def descentIdent(id: Identifier): String = id.name
}
