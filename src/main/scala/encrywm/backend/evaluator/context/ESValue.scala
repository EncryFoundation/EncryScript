package encrywm.backend.evaluator.context

import encrywm.builtins.Types.TYPE

sealed trait ESValue extends ESCtxComponent {

  val name: String
  val tpe: TYPE
  val value: tpe.Underlying
}

object ESValue {

  val typeId: Byte = 2.toByte

  def apply(n: String, t: TYPE)(v: t.Underlying): ESValue = new ESValue {
    override val name: String = n
    override val tpe: TYPE = t
    override val value: tpe.Underlying = v.asInstanceOf[tpe.Underlying]
  }
}
