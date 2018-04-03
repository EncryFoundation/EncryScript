package encrywm.frontend.semantics.scope

import encrywm.lib.Types.{ESBoolean, ESByteVector, ESContext}
import encrywm.lib.predef.functions.{Blake2b256Hash, CheckSig}

object ESPredefScope {

  val predefNames: Seq[Symbol] = Seq(
    ValSymbol("context", ESContext),
    FuncSymbol(CheckSig.name, ESBoolean, params = CheckSig.args.map(arg => ValSymbol(arg._1, arg._2))),
    FuncSymbol(Blake2b256Hash.name, ESByteVector, params = Blake2b256Hash.args.map(arg => ValSymbol(arg._1, arg._2)))
  )
}
