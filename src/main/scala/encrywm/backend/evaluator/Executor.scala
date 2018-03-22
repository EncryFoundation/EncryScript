package encrywm.backend.evaluator

import encrywm.ast.Ast._
import encrywm.backend.evaluator.context.{ESFunc, ESObject, ESValue, ScopedRuntimeContext}
import encrywm.utils.Stack

import scala.util.{Success, Try}

class Executor {

  import Executor._

  private var ctx: ScopedRuntimeContext = ScopedRuntimeContext.empty("GLOBAL", 1)

  private val ctxs: Stack[ScopedRuntimeContext] = new Stack

  def execute(node: AST_NODE): ExecOutcome = Try {

    def eval[T](expr: EXPR): T = {
      def checkType(v: Any): T = v match {
        case t: T => t
        case _ => throw EvaluationError("Unexpected type")
      }
      (expr match {
        case EXPR.Name(id, _, _) =>
          ctx.get(id.name).map {
            case v: ESValue =>
              checkType(v.value)
            case o: ESObject => checkType(o)
            case f: ESFunc => throw EvaluationError(s"${f.name} is function")
          }.getOrElse(throw EvaluationError("Unknown reference"))

        case EXPR.BinOp(l, op, r, tpeOpt) =>
          val opT = tpeOpt.get
          val leftT = l.tpeOpt.get
          val rightT = r.tpeOpt.get
          val leftV = eval[leftT.Underlying](l)
          val rightV = eval[rightT.Underlying](r)
          op match {
            case _: OPERATOR.Add.type =>
              val r = Math.sum[opT.Underlying](leftV, rightV)
              println(r)
              r
          }

        case EXPR.IntConst(v) => v

        case EXPR.LongConst(v) => v

        case EXPR.DoubleConst(v) => v

        case EXPR.FloatConst(v) => v

        case _ => throw EvaluationError("Unexpected expression")
      }).asInstanceOf[T]
    }

    def execMany(stmts: Seq[STMT]): ExecOutcome = {
      stmts.foreach { stmt =>
        exec(stmt) match {
          case Left(outcome) => return Left(outcome)
          case _ => // Do nothing
        }
      }
      Right(Locked)
    }

    def exec(stmt: STMT): ExecOutcome = stmt match {
      case asg: STMT.Assign =>
        val valT = asg.value.tpeOpt.get
        asg.target match {
          case EXPR.Decl(t, _) => t match {
            case EXPR.Name(id, _, _) =>
              ctx = ctx.updated(
                ESValue(id.name, valT)(eval[valT.Underlying](asg.value)))
              Right(Locked)
          }
        }
      case STMT.Expr(expr) =>
        val exprT = expr.tpeOpt.get
        eval[exprT.Underlying](expr)
        Right(Locked)

      case STMT.Unlock => Left(Unlocked)
    }

    node match {
      case TREE_ROOT.Contract(body) => execMany(body)
      case TREE_ROOT.Expression(inner) => execMany(inner)
    }
  } match {
    case Success(Left(o)) => Left(o)
    case _ => Right(Locked)
  }
}

object Executor {

  case object Unlocked

  case object Locked

  type ExecOutcome = Either[Unlocked.type, Locked.type]
}
