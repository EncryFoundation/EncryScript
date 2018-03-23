package encrywm.builtins.functions

import encrywm.backend.executor.context.{ESBuiltInFunc, ESCtxComponent, ESValue}
import encrywm.builtins.Types.BYTE_VECTOR
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

sealed trait ESBuiltInFunctionHolder {

  val fn: ESBuiltInFunc
}

//object CheckSig extends ESBuiltInFunctionHolder {
//
//  override val fn: ESBuiltInFunc = ESBuiltInFunc()
//
//  private val body = (args: Seq[(String, ESCtxComponent)]) => {
//    val validNumberOfArgs = args.size == 3
//    val validArgTypes = args.forall { case (_, v: ESValue) => v.tpe.isInstanceOf[BYTE_VECTOR.type] }
//    if (validNumberOfArgs && validArgTypes) {
//      val fnArgs = args.map(_._2.asInstanceOf[ESValue].value)
//      Right(Curve25519.verify(Signature @@ args.head.asInstanceOf[Array[Byte]],
//        fnArgs(1).asInstanceOf[Array[Byte],
//          PublicKey @@ fnArgs.))
//    }
//  }
//}
