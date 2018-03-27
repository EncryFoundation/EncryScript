package encrywm.backend.executor.context

import encrywm.core.Types.ESType

sealed trait ESValue extends ESRuntimeComponent {

  val name: String
  val tpe: ESType
  val value: tpe.Underlying
}

object ESValue {

  val typeId: Byte = 2.toByte

  def apply(n: String, t: ESType)(v: t.Underlying): ESValue = new ESValue {
    override val name: String = n
    override val tpe: ESType = t
    override val value: tpe.Underlying = v.asInstanceOf[tpe.Underlying]
  }
}
