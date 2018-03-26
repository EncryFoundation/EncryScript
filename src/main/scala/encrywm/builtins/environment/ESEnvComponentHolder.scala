package encrywm.builtins.environment

import encrywm.frontend.semantics.scope.Symbol

trait ESEnvComponentHolder {

  val name: String

  val symbol: Symbol
}
