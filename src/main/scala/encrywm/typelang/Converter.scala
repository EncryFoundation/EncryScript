package encrywm.typelang

import encrytl.core.{Schema, TypedObject}
import encrywm.lang.backend.env.{ESObject, ESValue}
import scorex.crypto.encode.Base58

import scala.util.Try

object Converter {

  import encrytl.core.Types._
  import encrywm.lib.Types._

  /** Converts `encrytl.core.Schema` to `ESTypedObject` */
  def schema2ESType(schema: Schema): Try[ESTypedObject] = Try {
    ESTypedObject(schema.ident, List("fingerprint" -> ESByteVector, "body" -> convertType(schema.tpe)))
  }

  /** Converts `encrytl.core.TypedObject` to `ESObject` */
  def typedObj2ESObj(obj: TypedObject): Try[ESObject] = Try {
    val fields = obj.fields.map { case (n, v) =>
      n -> ESValue(n, convertType(v.tpe))(v.value)
    }.toMap
    ESObject(Base58.encode(obj.fingerprint), fields, SDObject)
  }

  private def convertType(tpe: EType): ESType = tpe match {
    case _: EInt.type => ESInt
    case _: ELong.type => ESLong
    case _: EString.type => ESString
    case _: EBoolean.type => ESBoolean
    case _: EByteVector.type => ESByteVector
    case EList(inT) => ESList(convertType(inT))
    case p @ EProduct(fields) => ESTypedObject(Base58.encode(p.fingerprint), fields.map(f => f._1 -> convertType(f._2)))
    case _ => throw new Exception("Unresolved type")
  }
}
