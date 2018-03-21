package encrywm.backend.evaluator

import encrywm.ast.Ast._
import encrywm.backend.evaluator.context.ScopedRuntimeContext
import monix.eval.Coeval

class Evaluator {

  import Evaluator._

  private lazy val globalContext: ScopedRuntimeContext = ??? // Initialize global ctx.

  def evaluate(node: AST_NODE, context: ScopedRuntimeContext): EvalOutcome = {

    def eval[T](expr: EXPR, ctx: ScopedRuntimeContext): EvalResult[T] = expr match {
      case n: EXPR.Name => ???
      case _ =>
    }

    def evalMany(stmts: Seq[STMT]): EvalOutcome = {
      stmts.foreach { stmt =>
        evaluate(stmt, context) match {
          case Left(outcome) => return Left(outcome)
          case _ => // Do nothing
        }
      }
      Right(Locked)
    }

    node match {
      case root: TREE_ROOT => root match {
        case contract: TREE_ROOT.Contract =>
          evalMany(contract.body)
      }
      case asg: STMT.Assign => // How to mutate ctx??
    }
  }
}

object Evaluator {

  case object Unlocked

  case object Locked

  type EvalResult[T] = Either[Coeval[T], EvaluationError]

  type EvalOutcome = Either[Unlocked.type, Locked.type]
}
