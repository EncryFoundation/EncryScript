package encrywm.backend.executor

import encrywm.ast.Ast._
import encrywm.backend.executor.context.{ESFunc, ESObject, ESValue, ScopedRuntimeContext}

import scala.util.{Random, Success, Try}

class Executor {

  import Executor._

  private val ctx: ScopedRuntimeContext = ScopedRuntimeContext.empty("GLOBAL", 1)

  def executeContract(c: TREE_ROOT.Contract): ExecOutcome = execute(c.body)

  private def execute(statements: Seq[STMT], context: ScopedRuntimeContext = ctx): ExecOutcome = Try {

    var currentCtx = context

    def eval[T](expr: EXPR): T = {
      (expr match {
        case EXPR.Name(id, _, _) =>
          currentCtx.get(id.name).map {
            case v: ESValue => v.value
            case o: ESObject => o
            case f: ESFunc => throw ExecutionError(s"${f.name} is function")
          }.getOrElse(throw ExecutionError("Unknown reference"))

        case EXPR.BinOp(l, op, r, tpeOpt) =>
          val opT = tpeOpt.get
          val leftT = l.tpeOpt.get
          val rightT = r.tpeOpt.get
          val leftV = eval[leftT.Underlying](l)
          val rightV = eval[rightT.Underlying](r)
          op match {
            case _: OPERATOR.Add.type =>
              Arith.sum[opT.Underlying](leftV, rightV)
            case _: OPERATOR.Mult.type =>
              Arith.mul[opT.Underlying](leftV, rightV)
            case _: OPERATOR.Div.type =>
              Arith.div[opT.Underlying](leftV, rightV)
          }

        case EXPR.BoolOp(op, os) => op match {
          case BOOL_OP.And => ???
          case BOOL_OP.Or => ???
        }

        case EXPR.Compare(left, ops, comps) =>
          val leftT = left.tpeOpt.get
          val leftV = eval[leftT.Underlying](left)
          ops.zip(comps).forall {
            case (COMP_OP.Eq, comp) =>
              val compT = comp.tpeOpt.get
              Compare.eq(leftV, eval[compT.Underlying](comp))
            case (COMP_OP.Gt, comp) =>
              val compT = comp.tpeOpt.get
              Compare.gt(leftV, eval[compT.Underlying](comp))
            case (COMP_OP.GtE, comp) =>
              val compT = comp.tpeOpt.get
              Compare.gte(leftV, eval[compT.Underlying](comp))
            case (COMP_OP.Lt, comp) =>
              val compT = comp.tpeOpt.get
              Compare.lt(leftV, eval[compT.Underlying](comp))
            case (COMP_OP.LtE, comp) =>
              val compT = comp.tpeOpt.get
              Compare.lte(leftV, eval[compT.Underlying](comp))
          }

        case EXPR.Call(EXPR.Name(id, _, _), args, kwargs, tpeOpt) =>
          currentCtx.get(id.name).map {
            case f: ESFunc =>
              val nestedCtx = currentCtx.emptyChild(id.name) // TODO: Add args to ctx.
              execute(f.body, nestedCtx) match {
                case Right(Result(Val(v))) => v
                case Right(Result(Unlocked)) => ???
                case Right(Result(Halt)) => ???
              }
          }

        case EXPR.True => true

        case EXPR.False => false

        case EXPR.IntConst(v) => v

        case EXPR.LongConst(v) => v

        case EXPR.DoubleConst(v) => v

        case EXPR.FloatConst(v) => v

        case _ => throw ExecutionError("Unexpected expression")
      }).asInstanceOf[T]
    }

    def exec(stmt: STMT): ExecOutcome = stmt match {

      case STMT.Assign(EXPR.Decl(EXPR.Name(id, _, _), _), value) =>
        val valT = value.tpeOpt.get
        currentCtx = currentCtx.updated(
          ESValue(id.name, valT)(eval[valT.Underlying](value))
        )
        Left(ESUnit)

      case STMT.Expr(expr) =>
        val exprT = expr.tpeOpt.get
        eval[exprT.Underlying](expr)
        Left(ESUnit)

      case STMT.If(test, body, orelse) =>
        val testT = test.tpeOpt.get
        val nestedCtx = currentCtx.emptyChild(s"if_stmt_${Random.nextInt()}")
        eval[testT.Underlying](test) match {
          case true => execute(body, nestedCtx)
          case false => execute(orelse, nestedCtx)
        }

      case STMT.Unlock => Right(Result(Unlocked))

      case STMT.Halt => Right(Result(Halt))

      case STMT.Return(None) => Left(ESUnit)

      case STMT.Return(Some(v)) =>
        val valT = v.tpeOpt.get
        Right(Result(Val(eval[valT.Underlying](v))))
    }

    def execMany(stmts: Seq[STMT]): ExecOutcome = {
      for (stmt <- stmts) {
          exec(stmt) match {
          case Right(Result(u: Unlocked.type)) =>
            return Right(Result(u))
          case Right(Result(h: Halt.type)) =>
            return Right(Result(h))
          case Right(Result(Val(v))) =>
            return Right(Result(Val(v)))
          case _ => // Do nothing
        }
      }
      Left(ESUnit)
    }

    execMany(statements)
  } match {
    case Success(Right(out)) => Right(out)
    case _ => Left(ESUnit)
  }
}

object Executor {

  type ExecOutcome = Either[ESUnit.type, Result]

  case class Result(r: Any)

  case class Val(v: Any)

  case object Unlocked

  case object Halt

  case object ESUnit
}
