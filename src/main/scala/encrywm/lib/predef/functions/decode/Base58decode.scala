package encrywm.lib.predef.functions.decode

import encrywm.lang.backend.env.{ESBuiltInFunc, ESValue}
import encrywm.lang.backend.executor.error.BuiltInFunctionExecError
import encrywm.lib.Types.ESString
import encrywm.lib.predef.functions.BuiltInFunctionHolder
import scorex.crypto.encode.Base58

object Base58decode extends BuiltInFunctionHolder {

  val name: String = "decode"

  override def asFunc: ESBuiltInFunc = ESBuiltInFunc(name, args, body)

  val args = IndexedSeq("s" -> ESString)

  private val body = (args: Seq[(String, ESValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe.isInstanceOf[ESString.type] }
    if (validNumberOfArgs && validArgTypes) {
      val fnArg = args.map(_._2.value.asInstanceOf[String]).head
      Right(Base58.decode(fnArg).toOption)
    } else {
      Left(BuiltInFunctionExecError)
    }
  }
}
