package encrywm.backend.evaluator.context.predefindFunctions

import encrywm.backend.evaluator.EvaluationError
import encrywm.backend.evaluator.context.{ESPredFunc, ESValue}
import encrywm.builtins.Types.BOOLEAN

import scala.util.Try

case class IsInstanceOf() extends ESPredFunc {

  override val name: String = "IsInstanceOf"

  override val eval: Seq[ESValue] => Try[ESValue] = (args: Seq[ESValue]) => Try{

    if(args.length != 2) throw EvaluationError("Incorrect args count for IsInstanceOf()")

    ESValue("IsInstanceOf", BOOLEAN)(args.head.tpe == args.last.tpe)
  }

}
