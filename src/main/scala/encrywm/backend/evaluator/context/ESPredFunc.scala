package encrywm.backend.evaluator.context

import encrywm.ast.Ast.AST_NODE
import encrywm.builtins.Types.TYPE

import scala.util.Try

trait ESPredFunc{

  val name: String

  val eval: Seq[ESValue] => Try[ESValue]

}
