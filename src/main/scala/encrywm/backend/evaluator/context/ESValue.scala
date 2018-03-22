package encrywm.backend.evaluator.context

import encrywm.backend.evaluator.Executor.EvalResult
import encrywm.builtins.Types.TYPE
import monix.eval.Coeval

sealed trait ESValue extends ESCtxComponent {

  val name: String
  val tpe: TYPE
  val value: EvalResult[tpe.Underlying]
}

object ESValue {

  val typeId: Byte = 2.toByte

  def apply(n: String, t: TYPE)(v: EvalResult[t.Underlying]): ESValue = new ESValue {
    override val name: String = n
    override val tpe: TYPE = t
    override val value: EvalResult[tpe.Underlying] = Right(Coeval.evalOnce(v.asInstanceOf[tpe.Underlying]))
  }
}
