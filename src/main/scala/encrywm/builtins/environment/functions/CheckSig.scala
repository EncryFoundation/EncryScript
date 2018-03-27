package encrywm.builtins.environment.functions

import encrywm.backend.executor.context.{ESBuiltInFunc, ESValue}
import encrywm.backend.executor.error.BuiltInFunctionExecError
import encrywm.builtins.Types
import encrywm.builtins.Types.ESByteVector
import encrywm.frontend.semantics.scope.{FuncSymbol, ValSymbol}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

object CheckSig extends ESBuiltInFunctionHolder {

  val name: String = "checkSig"

  override lazy val fn: ESBuiltInFunc = ESBuiltInFunc(name, args, body)

  private val args = IndexedSeq("msg" -> ESByteVector, "sig" -> ESByteVector, "pubKey" -> ESByteVector)

  private val body = (args: Seq[(String, ESValue)]) => {
    val validNumberOfArgs = args.size == 3
    val validArgTypes = args.forall { case (_, v) => v.tpe.isInstanceOf[ESByteVector.type] }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = args.map(_._2.value.asInstanceOf[Array[Byte]])
      Right(Curve25519.verify(Signature @@ fnArgs.head, fnArgs(1), PublicKey @@ fnArgs.last))
    } else {
      Left(BuiltInFunctionExecError)
    }
  }
}
