package encrywm.frontend.semantics.scope

import encrywm.lib.Types.{ESBoolean, ESByteVector, ESContext, ESFunc}
import encrywm.lib.predef.functions.hash._
import encrywm.lib.predef.functions.signature._
import encrywm.lib.predef.functions.time._

object ESPredefScope {

  val predefNames: Seq[Symbol] = Seq(
    ValSymbol("context", ESContext),
    FuncSymbol(CheckSig.name, ESBoolean, params = CheckSig.args.map(arg => ValSymbol(arg._1, arg._2))),
    FuncSymbol(Blake2b256Hash.name, ESByteVector, params = Blake2b256Hash.args.map(arg => ValSymbol(arg._1, arg._2))),
    FuncSymbol(Blake2b512Hash.name, ESByteVector, params = Blake2b512Hash.args.map(arg => ValSymbol(arg._1, arg._2))),
    FuncSymbol(Keccak256Hash.name, ESByteVector, params = Keccak256Hash.args.map(arg => ValSymbol(arg._1, arg._2))),
    FuncSymbol(Keccak512Hash.name, ESByteVector, params = Keccak512Hash.args.map(arg => ValSymbol(arg._1, arg._2))),
    FuncSymbol(Sha256Hash.name, ESByteVector, params = Sha256Hash.args.map(arg => ValSymbol(arg._1, arg._2))),
    FuncSymbol(Str2Time.name, ESByteVector, params = Str2Time.args.map(arg => ValSymbol(arg._1, arg._2))),
    FuncSymbol("exists", ESBoolean, params = IndexedSeq(ValSymbol("predicate", ESFunc(ESBoolean))))
  )
}
