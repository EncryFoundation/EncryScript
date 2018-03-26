package encrywm.builtins

import encrywm.backend.executor.context.ESObject
import encrywm.frontend.semantics.scope.BuiltInTypeSymbol

object Types {

  sealed trait TYPE {
    type Underlying
    val identifier: String
    lazy val symbol: BuiltInTypeSymbol = BuiltInTypeSymbol(identifier)

    override def equals(obj: scala.Any): Boolean = obj match {
      case s: STATIC => this.identifier == s.identifier
      case sym: BuiltInTypeSymbol => this.identifier == sym.name
      case _ => false
    }
  }

  sealed trait STATIC extends TYPE

  sealed trait PARAMETRIZED extends TYPE

  // Primitives
  case object UNIT extends TYPE with STATIC {
    override type Underlying = Unit
    override val identifier: String = "unit"
  }
  case object BOOLEAN extends TYPE with STATIC {
    override type Underlying = Boolean
    override val identifier: String = "bool"
  }
  case object INT extends TYPE with STATIC {
    override type Underlying = Int
    override val identifier: String = "int"
  }
  case object LONG extends TYPE with STATIC {
    override type Underlying = Long
    override val identifier: String = "long"
  }
  case object FLOAT extends TYPE with STATIC {
    override type Underlying = Float
    override val identifier: String = "float"
  }
  case object DOUBLE extends TYPE with STATIC {
    override type Underlying = Double
    override val identifier: String = "double"
  }
  case object STRING extends TYPE with STATIC {
    override type Underlying = String
    override val identifier: String = "string"
  }
  case object BYTE_VECTOR extends TYPE with STATIC {
    override type Underlying = Array[Byte]  // TODO: Switch to scodec.Bytevector
    override val identifier: String = "bytes"
  }

  // Complex types
  case class LIST(valT: TYPE) extends TYPE with PARAMETRIZED {
    override type Underlying = List[valT.Underlying]
    override val identifier: String = "list"

    override def equals(obj: Any): Boolean = obj match {
      case l: LIST => l.valT == this.valT
      case sym: BuiltInTypeSymbol => this.identifier == sym.name &&
        sym.typeParams.headOption.exists(_.name == this.valT.identifier)
      case _ => false
    }
  }
  case class DICT(keyT: TYPE, valT: TYPE) extends TYPE with PARAMETRIZED {
    override type Underlying = Map[keyT.Underlying, valT.Underlying]
    override val identifier: String = "dict"

    override def equals(obj: Any): Boolean = obj match {
      case d: DICT => d.keyT == this.keyT && d.valT == this.valT
      case sym: BuiltInTypeSymbol => this.identifier == sym.name &&
        sym.typeParams.headOption.exists(_.name == this.keyT.identifier) &&
        sym.typeParams.lastOption.exists(_.name == this.valT.identifier)
      case _ => false
    }
  }
  case class OPTION(inT: TYPE) extends TYPE with PARAMETRIZED {
    override type Underlying = Option[inT.Underlying]
    override val identifier: String = "option"
  }

  case class TYPE_REF(identifier: String) extends TYPE {
    override type Underlying = ESObject

    override def equals(obj: scala.Any): Boolean = obj match {
      case tr: TYPE_REF => this.identifier == tr.identifier
      case sym: BuiltInTypeSymbol => this.identifier == sym.name
      case _ => false
    }
  }

  lazy val staticTypes: Map[String, STATIC] = Map(
    UNIT.identifier -> UNIT,
    BOOLEAN.identifier -> BOOLEAN,
    INT.identifier -> INT,
    LONG.identifier -> LONG,
    FLOAT.identifier -> FLOAT,  // TODO: Remove float?
    DOUBLE.identifier -> DOUBLE,
    STRING.identifier -> STRING,
    BYTE_VECTOR.identifier -> BYTE_VECTOR,
  )

  def staticTypeById(id: String): Option[TYPE] = staticTypes.get(id)
}
