package encrywm.lib.predef.hash

import encrywm.lang.backend.env.{ESBuiltInFunc, ESValue}
import encrywm.lang.backend.executor.error.BuiltInFunctionExecException
import encrywm.lib.Types.ESByteVector
import encrywm.lib.predef.BuiltInFunctionHolder
import scorex.crypto.hash.Keccak256

object Keccak256Hash extends BuiltInFunctionHolder {

  val name = "keccak256hash"

  override def asFunc: ESBuiltInFunc = ESBuiltInFunc(name, args, body)

  val args = IndexedSeq("input" -> ESByteVector)

  private val body = (args: Seq[(String, ESValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe.isInstanceOf[ESByteVector.type] }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = args.map(_._2.value.asInstanceOf[Array[Byte]])
      Right(Keccak256.hash(fnArgs.head))
    } else {
      Left(BuiltInFunctionExecException)
    }
  }
}