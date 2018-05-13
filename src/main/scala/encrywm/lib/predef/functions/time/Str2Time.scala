package encrywm.lib.predef.functions.time

import java.text.SimpleDateFormat

import encrywm.lang.backend.env.{ESBuiltInFunc, ESValue}
import encrywm.lang.backend.executor.error.BuiltInFunctionExecError
import encrywm.lib.Types.ESString
import encrywm.lib.predef.functions.BuiltInFunctionHolder

object Str2Time extends BuiltInFunctionHolder {

  val name = "unixTime"

  override def asFunc: ESBuiltInFunc = ESBuiltInFunc(name, args, body)

  val args = IndexedSeq("input" -> ESString)

  private val body = (args: Seq[(String, ESValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe.isInstanceOf[ESString.type] }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = args.map(_._2.value.asInstanceOf[String])
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
      Right(dateFormat.parse(fnArgs.head).getTime)
    } else {
      Left(BuiltInFunctionExecError)
    }
  }
}
