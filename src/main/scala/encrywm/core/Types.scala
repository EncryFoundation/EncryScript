package encrywm.core

import encrywm.backend.executor.context.ESObject

object Types {

  sealed trait ESType {
    type Underlying
    val identifier: String

    def isPrimitive: Boolean = this.isInstanceOf[ESPrimitive]

    override def equals(obj: scala.Any): Boolean = obj match {
      case s: ESPrimitive => this.identifier == s.identifier
      case _ => false
    }
  }

  sealed trait ESPrimitive extends ESType

  case object ESUnit extends ESType with ESPrimitive {
    override type Underlying = Unit
    override val identifier: String = "Unit"
  }
  case object ESBoolean extends ESType with ESPrimitive {
    override type Underlying = Boolean
    override val identifier: String = "Bool"
  }
  case object ESInt extends ESType with ESPrimitive {
    override type Underlying = Int
    override val identifier: String = "Int"
  }
  case object ESLong extends ESType with ESPrimitive {
    override type Underlying = Long
    override val identifier: String = "Long"
  }
  case object FLOAT extends ESType with ESPrimitive {
    override type Underlying = Float
    override val identifier: String = "Float"
  }
  case object DOUBLE extends ESType with ESPrimitive {
    override type Underlying = Double
    override val identifier: String = "Double"
  }
  case object ESString extends ESType with ESPrimitive {
    override type Underlying = String
    override val identifier: String = "String"
  }
  case object ESByteVector extends ESType with ESPrimitive {
    override type Underlying = Array[Byte]  // TODO: Switch to scodec.Bytevector
    override val identifier: String = "Bytes"
  }

  sealed trait ESProduct extends ESType {
    val fields: Map[String, ESType] = Map.empty

    def getAttrType(n: String): Option[ESType] = fields.get(n)

    def typeOfField(fn: String): Option[ESType] = fields.get(fn)

    override def equals(obj: Any): Boolean = obj match {
      case p: ESProduct =>
        if (p.fields.size != this.fields.size) false
        else p.fields.zip(this.fields).forall { case ((f1, _), (f2, _)) => f1 == f2 }
    }
  }

  case object ESTransaction extends ESType with ESProduct {
    override type Underlying = ESObject
    override val identifier: String = "Transaction"

    override val fields: Map[String, ESType] = Map(
      "accountPubKey" -> ESByteVector,
      "timestamp" -> ESLong,
      "signature" -> ESByteVector,
      "bodyBytes" -> ESByteVector
    )
  }

  case object ESState extends ESType with ESProduct {
    override type Underlying = ESObject
    override val identifier: String = "State"

    override val fields: Map[String, ESType] = Map(
      "height" -> ESLong,
      "lastBlockTimestamp" -> ESLong,
      "stateDigest" -> ESByteVector
    )
  }

  sealed trait Parametrized

  sealed trait ESCollection extends ESProduct with Parametrized

  case class ESList(valT: ESType) extends ESType with ESCollection {
    override type Underlying = List[valT.Underlying]
    override val identifier: String = "List"
    override val fields: Map[String, ESType] = ESList.fields

    override def equals(obj: Any): Boolean = obj match {
      case l: ESList => l.valT == this.valT
      case _ => false
    }
  }
  object ESList {
    val fields: Map[String, ESType] = Map(
      "size" -> ESInt
    )
  }

  case class ESDict(keyT: ESType, valT: ESType) extends ESType with ESCollection {
    override type Underlying = Map[keyT.Underlying, valT.Underlying]
    override val identifier: String = "Dict"
    override val fields: Map[String, ESType] = ESDict.fields

    override def equals(obj: Any): Boolean = obj match {
      case d: ESDict => d.keyT == this.keyT && d.valT == this.valT
      case _ => false
    }
  }
  object ESDict {
    val fields: Map[String, ESType] = Map(
      "size" -> ESInt
    )
  }

  case class ESOption(inT: ESType) extends ESType with ESProduct with Parametrized {
    override type Underlying = Option[inT.Underlying]
    override val identifier: String = "Option"
  }

  // Placeholder fot inferred type.
  case object NIType extends ESType {
    override type Underlying = Nothing
    override val identifier: String = "NotInferred"
  }

  lazy val primitiveTypes: Seq[ESPrimitive] = Seq(ESUnit, ESBoolean, ESInt, ESLong, ESString, ESByteVector)
  lazy val productTypes: Seq[ESProduct] = Seq(ESTransaction, ESOption(NIType))
  lazy val collTypes: Seq[ESCollection] = Seq(ESDict(NIType, NIType), ESList(NIType))

  lazy val allTypes: Seq[ESType] = primitiveTypes ++ productTypes ++ collTypes

  lazy val typesMap: Map[String, ESType] = allTypes.map(t => t.identifier -> t).toMap

  def typeByIdent(id: String): Option[ESType] = typesMap.get(id)

  def liftType(d: Any): ESType = d match {
    case _: Int => ESInt
    case _: Long => ESLong
    case _: Boolean => ESBoolean
    case _: String => ESString
    case _: Array[Byte] => ESByteVector
  }
}
