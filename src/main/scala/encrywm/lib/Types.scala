package encrywm.lib

import encrywm.backend.env.ESObject

object Types {

  sealed trait ESType {
    type Underlying
    val ident: String

    def isPrimitive: Boolean = this.isInstanceOf[ESPrimitive]

    def isCollection: Boolean = this.isInstanceOf[ESCollection]

    def isOption: Boolean = this.isInstanceOf[ESOption]

    def isProduct: Boolean = this.isInstanceOf[ESProduct]

    def isFunc: Boolean = this.isInstanceOf[ESFunc]

    override def equals(obj: scala.Any): Boolean = obj match {
      case s: ESPrimitive => s.ident == this.ident
      case p: ESProduct => p == this
      case _ => false
    }
  }

  sealed trait ESPrimitive extends ESType

  case object ESUnit extends ESType with ESPrimitive {
    override type Underlying = Unit
    override val ident: String = "Unit"
  }
  case object ESBoolean extends ESType with ESPrimitive {
    override type Underlying = Boolean
    override val ident: String = "Bool"
  }
  case object ESInt extends ESType with ESPrimitive {
    override type Underlying = Int
    override val ident: String = "Int"
  }
  case object ESLong extends ESType with ESPrimitive {
    override type Underlying = Long
    override val ident: String = "Long"
  }
  case object FLOAT extends ESType with ESPrimitive {
    override type Underlying = Float
    override val ident: String = "Float"
  }
  case object DOUBLE extends ESType with ESPrimitive {
    override type Underlying = Double
    override val ident: String = "Double"
  }
  case object ESString extends ESType with ESPrimitive {
    override type Underlying = String
    override val ident: String = "String"
  }
  case object ESByteVector extends ESType with ESPrimitive {
    override type Underlying = Array[Byte]
    override val ident: String = "Bytes"
  }

  sealed trait ESProduct extends ESType {
    val superTypeOpt: Option[ESProduct] = None
    def fields: Map[String, ESType] = superTypeOpt.map(_.fields).getOrElse(Map.empty)

    // TODO: Write test.
    def getAttrType(n: String): Option[ESType] = fields.get(n)
      .orElse(superTypeOpt.flatMap(_.getAttrType(n)))

    def typeOfField(fn: String): Option[ESType] = fields.get(fn)

    def isSubtypeOf(thatT: ESType): Boolean =
      superTypeOpt.exists(parT => parT == thatT || parT.isSubtypeOf(thatT))

    override def equals(obj: Any): Boolean = obj match {
      case p: ESProduct =>
        if (p.fields.size != this.fields.size) false
        else p.fields.zip(this.fields).forall { case ((f1, _), (f2, _)) => f1 == f2 }
      case _ => false
    }
  }

  case object ESContext extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "Context"

    override val fields: Map[String, ESType] = Map(
      "proof" -> ESProof,
      "transaction" -> ESTransaction,
      "state" -> ESState
    )
  }

  // Abstract type
  case object ESProof extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "Proof"

    override val fields: Map[String, ESType] = Map(
      "typeId" -> ESInt
    )
  }

  // ESProof impl
  case object Signature25519 extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "Signature25519"

    override val superTypeOpt: Option[ESProduct] = Some(ESProof)

    override val fields: Map[String, ESType] = Map(
      "sigBytes" -> ESByteVector
    )
  }

  // Abstract type
  case object ESProposition extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "Proposition"

    override val fields: Map[String, ESType] = Map(
      "typeId" -> ESInt
    )
  }

  // ESProposition impl
  case object AccountProposition extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "AccountProposition"

    override val superTypeOpt: Option[ESProduct] = Some(ESProposition)

    override val fields: Map[String, ESType] = Map(
      "accountAddress" -> ESByteVector
    )
  }

  // Abstract type
  case object ESBox extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "Box"

    override val fields: Map[String, ESType] = Map(
      "proposition" -> ESProposition,
      "typeId" -> ESInt,
      "id" -> ESByteVector
    )
  }

  // ESBox impl
  case object AssetBox extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "AssetBox"

    override val superTypeOpt: Option[ESProduct] = Some(ESBox)

    override val fields: Map[String, ESType] = Map(
      "amount" -> ESLong
    )
  }

  case object ESUnlocker extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "Unlocker"

    override val fields: Map[String, ESType] = Map(
      "boxId" -> ESTransaction,
      "proofOpt" -> ESOption(ESProof)
    )
  }

  case object ESTransaction extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "Transaction"

    override val fields: Map[String, ESType] = Map(
      "accountPubKey" -> ESByteVector,
      "fee" -> ESLong,
      "timestamp" -> ESLong,
      "signature" -> ESByteVector,
      "unlockers" -> ESList(ESUnlocker),
      "messageToSign" -> ESByteVector
    )
  }

  case object ESState extends ESType with ESProduct {
    override type Underlying = ESObject
    override val ident: String = "State"

    override val fields: Map[String, ESType] = Map(
      "height" -> ESLong,
      "lastBlockTimestamp" -> ESLong,
      "stateDigest" -> ESByteVector
    )
  }

  sealed trait Parametrized

  sealed trait ESCollection extends ESProduct with Parametrized {

    override def fields: Map[String, ESType] = Map(
      "size" -> ESInt,
      "exists" -> ESBoolean
    )
  }

  case class ESList(valT: ESType) extends ESType with ESCollection {
    override type Underlying = List[valT.Underlying]
    override val ident: String = "List"

    override def equals(obj: Any): Boolean = obj match {
      case l: ESList => l.valT == this.valT
      case _ => false
    }
  }

  case class ESDict(keyT: ESType, valT: ESType) extends ESType with ESCollection {
    override type Underlying = Map[keyT.Underlying, valT.Underlying]
    override val ident: String = "Dict"

    override def equals(obj: Any): Boolean = obj match {
      case d: ESDict => d.keyT == this.keyT && d.valT == this.valT
      case _ => false
    }
  }

  case class ESOption(inT: ESType) extends ESType with ESProduct with Parametrized {
    override type Underlying = Option[inT.Underlying]
    override val ident: String = "Option"
    override val fields: Map[String, ESType] = ESOption.fields ++ Map("get" -> inT)
  }

  object ESOption {
    val fields: Map[String, ESType] = Map(
      "isDefined" -> ESBoolean
    )
  }

  // TODO: Add attrs for deeper type checking.
  case class ESFunc(retT: ESType) extends ESType {
    override type Underlying = retT.Underlying
    override val ident: String = "func"

    override def equals(obj: Any): Boolean = obj match {
      case f: ESFunc => this.retT == f.retT
      case _ => false
    }
  }

  // Placeholder for not inferred type.
  case object NIType extends ESType {
    override type Underlying = Nothing
    override val ident: String = "NotInferred"
  }

  lazy val primitiveTypes: Seq[ESPrimitive] = Seq(
    ESUnit,
    ESBoolean,
    ESInt,
    ESLong,
    ESString,
    ESByteVector
  )

  lazy val productTypes: Seq[ESProduct] = Seq(
    ESTransaction,
    ESProof,
    ESProposition,
    ESContext,
    ESBox,
    Signature25519,
    AssetBox,
    AccountProposition,
    ESOption(NIType)
  )

  lazy val collTypes: Seq[ESCollection] = Seq(
    ESDict(NIType, NIType),
    ESList(NIType)
  )

  lazy val allTypes: Seq[ESType] = primitiveTypes ++ productTypes ++ collTypes :+ ESFunc(NIType)

  lazy val typesMap: Map[String, ESType] = allTypes.map(t => t.ident -> t).toMap

  def typeByIdent(id: String): Option[ESType] = typesMap.get(id)

  def liftType(d: Any): ESType = d match {
    case _: Int => ESInt
    case _: Long => ESLong
    case _: Boolean => ESBoolean
    case _: String => ESString
    case _: Array[Byte] => ESByteVector
  }
}
