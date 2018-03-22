package encrywm.backend.evaluator

import encrywm.ast.Ast._
import encrywm.backend.evaluator.context.{ESValue, ScopedRuntimeContext}
import encrywm.builtins.ESMath
import monix.eval.Coeval

import scala.util.{Success, Try}

class Executor {

  import Executor._

  private lazy val globalContext: ScopedRuntimeContext = ??? // Initialize global ctx.

  def execute(node: AST_NODE, context: ScopedRuntimeContext): ExecOutcome = Try {

    var currentCtx = context

    def eval[T](expr: EXPR): EvalResult[T] = {
      def checkType(v: Any): EvalResult[T] = v match {
        case t: T => Right(Coeval.evalOnce(t))
        case _ => Left(EvaluationError("Unexpected type"))
      }

      expr match {
        case n: EXPR.Name => currentCtx.get(n.id.name).map {
            case v: ESValue => checkType(v.value)
          }.getOrElse(Left(EvaluationError("Unknown reference")))

        case EXPR.BinOp(l, op, r, tpeOpt) =>
          val opT = tpeOpt.get
          val leftT = l.tpeOpt.get
          val rightT = r.tpeOpt.get
          val leftV = eval[leftT.Underlying](l)
          val rightV = eval[rightT.Underlying](r)
          op match {
            case add: OPERATOR.Add.type =>
              val r = leftV.flatMap(lv => rightV.map(rv => ESMath.sum[opT.Underlying](lv.apply(), rv.apply())))
              ???
          }

        case _ =>
      }
    }

    def execMany(stmts: Seq[STMT]): ExecOutcome = {
      stmts.foreach { stmt =>
        execute(stmt, context) match {
          case Left(outcome) => return Left(outcome)
          case _ => // Do nothing
        }
      }
      Right(Locked)
    }

    node match {
      case root: TREE_ROOT => root match {
        case contract: TREE_ROOT.Contract =>
          execMany(contract.body)
      }
      case asg: STMT.Assign => asg.target match {
        case n: EXPR.Name =>
          currentCtx = currentCtx.updated(
            ESValue(n.id.name, asg.value.tpeOpt.getOrElse(throw EvaluationError("Undefined type"))))
          Right(Locked)
      }
    }
  } match {
    case Success(Left(o)) => Left(o)
    case _ => Right(Locked)
  }
}

object Executor {

  case object Unlocked

  case object Locked

  type EvalResult[T] = Either[EvaluationError, Coeval[T]]

  type ExecOutcome = Either[Unlocked.type, Locked.type]
}
