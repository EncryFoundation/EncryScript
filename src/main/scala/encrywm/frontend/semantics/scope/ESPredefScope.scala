package encrywm.frontend.semantics.scope

import encrywm.core.Types.{ESBoolean, ESContext}
import encrywm.core.environment.functions.CheckSig

object ESPredefScope {

  val predefNames: Seq[Symbol] = Seq(
    ValSymbol("context", ESContext),
    FuncSymbol("checkSig", ESBoolean, params = CheckSig.args.map(arg => ValSymbol(arg._1, arg._2)))
  )
}
