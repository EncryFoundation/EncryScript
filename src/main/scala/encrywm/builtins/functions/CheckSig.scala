package encrywm.builtins.functions

import encrywm.backend.executor.context.{ESBuiltInFunc, ESCtxComponent, ESValue}
import encrywm.backend.executor.error.BuiltInFunctionExecError
import encrywm.builtins.Types.BYTE_VECTOR
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

sealed trait ESBuiltInFunctionHolder {

  val fn: ESBuiltInFunc
}

object CheckSig extends ESBuiltInFunctionHolder {

  override lazy val fn: ESBuiltInFunc = ESBuiltInFunc("checkSig", args, body)

  private val args = IndexedSeq("msg" -> BYTE_VECTOR, "sig" -> BYTE_VECTOR, "pubKey" -> BYTE_VECTOR)

  private val body = (args: Seq[(String, ESValue)]) => {
    val validNumberOfArgs = args.size == 3
    val validArgTypes = args.forall { case (_, v) => v.tpe.isInstanceOf[BYTE_VECTOR.type] }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = args.map(_._2.value.asInstanceOf[Array[Byte]])
      Right(Curve25519.verify(Signature @@ fnArgs.head, fnArgs(1), PublicKey @@ fnArgs.last))
    } else {
      Left(BuiltInFunctionExecError)
    }
  }
}
