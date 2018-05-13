package encrywm.tl

import encrytl.core.Schema
import scorex.crypto.encode.Base58

import scala.util.Try

object SchemaConverter {

  import encrytl.core.Types._
  import encrywm.lib.Types._

  def schema2ESType(schema: Schema): Try[ESTypedObject] = Try {
    ESTypedObject(schema.ident, List("fingerprint" -> ESByteVector, "body" -> convertType(schema.tpe)))
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
