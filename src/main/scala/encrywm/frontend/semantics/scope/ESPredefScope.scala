package encrywm.frontend.semantics.scope

import encrywm.core.Types.{ESBoolean, ESState, ESTransaction}
import encrywm.core.environment.functions.CheckSig

object ESPredefScope {

  val predefNames: Seq[Symbol] = Seq(
    ValSymbol("transaction", ESTransaction),
    ValSymbol("state", ESState),
    FuncSymbol("checkSig", ESBoolean, params = CheckSig.args.map(arg => ValSymbol(arg._1, arg._2)))
  )
}
