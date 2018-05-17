package encrywm.lib.predef.functions.decode

import encrytl.core.TypedObjectCodec
import encrywm.lang.backend.env.{ESBuiltInFunc, ESObject, ESValue}
import encrywm.lang.backend.executor.error.BuiltInFunctionExecException
import encrywm.lib.Types.ESByteVector
import encrywm.lib.predef.functions.BuiltInFunctionHolder
import encrywm.typelang.Converter

import scala.util.Try

/** EncryTL serialized data decoding. */
object SchemaDecode extends BuiltInFunctionHolder {

  val name: String = "read"

  override def asFunc: ESBuiltInFunc = ESBuiltInFunc(name, args, body)

  val args = IndexedSeq("data" -> ESByteVector)

  private val body = (args: Seq[(String, ESValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe == ESByteVector }
    if (validNumberOfArgs && validArgTypes) {
      val fnArg = args.map(_._2.value.asInstanceOf[Array[Byte]]).head
      Right(decodeData(fnArg).toOption)
    } else {
      Left(BuiltInFunctionExecException)
    }
  }

  private def decodeData(data: Array[Byte]): Try[ESObject] = TypedObjectCodec.decode(data).flatMap(Converter.typedObj2ESObj)
}
