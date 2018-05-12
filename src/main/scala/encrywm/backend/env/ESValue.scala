package encrywm.backend.env

import encrywm.lib.Types.ESType

sealed trait ESValue extends ESEnvComponent {

  val id: String
  val tpe: ESType
  val value: tpe.Underlying

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ESValue => that.value == this.value
    case _ => false
  }
}

object ESValue {

  val typeId: Byte = 2.toByte

  def apply(n: String, t: ESType)(v: Any): ESValue = new ESValue {
    override val id: String = n
    override val tpe: ESType = t
    override val value: tpe.Underlying = v.asInstanceOf[tpe.Underlying]
  }
}
