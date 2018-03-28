package encrywm.backend.executor

import encrywm.ast.Ast.EXPR.{IntConst, LongConst}
import encrywm.ast.Ast._
import encrywm.backend.env._
import encrywm.backend.executor.error._
import encrywm.backend.{Arith, Compare}
import encrywm.core.Types
import encrywm.core.Types.{ESDict, ESList}
import scorex.crypto.encode.Base58

import scala.util.{Failure, Random, Success, Try}

// TODO: Throw single error type inside the executor?
class Executor(globalContext: ScopedRuntimeEnv) {

  import Executor._

  private val ctx: ScopedRuntimeEnv = globalContext

  def executeContract(c: TREE_ROOT.Contract): ExecOutcome = execute(c.body)

  private def execute(statements: Seq[STMT],
                      context: ScopedRuntimeEnv = ctx): ExecOutcome = Try {

    var currentCtx = context

    def eval[T](expr: EXPR): T = {
      (expr match {
        case EXPR.Name(id, _, _) =>
          currentCtx.get(id.name).map {
            case v: ESValue => v.value
            case o: ESObject => o
            case _: ESFunc => throw IsFunctionError(id.name)
          }.getOrElse(throw UnresolvedReferenceError(id.name))

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

        case EXPR.BoolOp(op, operands) => op match {
          case BOOL_OP.And => operands.forall(eval[Boolean])
          case BOOL_OP.Or => operands.foldLeft(false) { case (bool, operand) =>
            bool || eval[Boolean](operand)
          }
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

        case EXPR.Call(EXPR.Name(id, _, _), args, kwargs, _) =>
          currentCtx.get(id.name).map {
            case ESFunc(_, fnArgs, _, body) =>
              val argMap = args.zip(fnArgs).map { case (exp, (argN, _)) =>
                val expT = exp.tpeOpt.get
                val expV = eval[expT.Underlying](exp)
                ESValue(argN, expT)(expV)
              }.map(v => v.name -> v).toMap
              val nestedCtx =
                ScopedRuntimeEnv(id.name, currentCtx.level + 1, argMap) // TODO: Add kwargs.
              execute(body, nestedCtx) match {
                case Right(Result(Val(v))) => v
                case Right(Result(Unlocked)) => throw UnlockException
                case Right(Result(Halt)) => throw ExecAbortException
                case _ => // Do nothing.
              }

            case ESBuiltInFunc(_, dArgs, body) =>
              val fnArgs = args.zip(dArgs).map { case (arg, (n, _)) =>
                val argT = arg.tpeOpt.get
                val argV = eval[argT.Underlying](arg)
                n -> ESValue(n, argT)(argV)
              }
              body(fnArgs) match {
                case Right(r) => r
                case _ => throw BuiltInFunctionExecError
              }

            case other => throw NotAFunctionError(other.toString)
          }.getOrElse(throw UnresolvedReferenceError(id.name))

        case EXPR.Attribute(value, attr, _, Some(_)) =>
          val valT = value.tpeOpt.get
          eval[valT.Underlying](value) match {
            case obj: ESObject => obj.getAttr(attr.name).get.value
            case _ => throw IllegalOperationError
          }

        case EXPR.IfExp(test, body, orelse, Some(tpe)) =>
          if (eval[Boolean](test)) {
            eval[tpe.Underlying](body)
          } else {
            eval[tpe.Underlying](orelse)
          }

        case EXPR.UnaryOp(op, operand, Some(_)) =>
          op match {
            case UNARY_OP.Not => !eval[Boolean](operand)
            case UNARY_OP.Invert => operand match {
              case exp: IntConst => eval[Int](exp) * (-1)
              case exp: LongConst => eval[Long](exp) * (-1)
            }
            case _ => throw IllegalOperationError
          }

        case EXPR.Subscript(exp, slice, _, Some(_)) =>
          val expT = exp.tpeOpt.get
          slice match {
            case SLICE.Index(idx) =>
              val idxT = idx.tpeOpt.get
              eval[expT.Underlying](exp) match {
                case lst: List[idxT.Underlying@unchecked] =>
                  lst(eval[Int](idx))
                case dct: Map[idxT.Underlying@unchecked, _] =>
                  dct(eval[idxT.Underlying](idx))
                case _ => throw IllegalOperationError
              }

            case _ => throw IllegalOperationError
          }

        case EXPR.ESList(elts, _, Some(ESList(valT))) =>
          elts.foldLeft(List[valT.Underlying]()) { case (acc, exp) =>
            acc :+ eval[valT.Underlying](exp)
          }

        case EXPR.ESDictNode(keys, values, Some(ESDict(keyT, valT))) =>
          keys.zip(values).foldLeft(Map[keyT.Underlying, valT.Underlying]()) { case (acc, (k, v)) =>
            acc.updated(eval[keyT.Underlying](k), eval[valT.Underlying](v))
          }

        case EXPR.Base58Str(s) => Base58.decode(s).get

        case EXPR.Str(s) => s

        case EXPR.True => true

        case EXPR.False => false

        case EXPR.IntConst(v) => v

        case EXPR.LongConst(v) => v

        case EXPR.DoubleConst(v) => v

        case EXPR.FloatConst(v) => v

        case exp => throw UnexpectedExpressionError(exp.toString)
      }).asInstanceOf[T]
    }

    def exec(stmt: STMT): ExecOutcome = stmt match {

      case STMT.Assign(EXPR.Declaration(EXPR.Name(id, _, _), _), value) =>
        val valT = value.tpeOpt.get
        currentCtx = currentCtx.updated(
          ESValue(id.name, valT)(eval[valT.Underlying](value))
        )
        Left(ESUnit)

      case STMT.Expr(expr) =>
        val exprT = expr.tpeOpt.get
        eval[exprT.Underlying](expr)
        Left(ESUnit)

      case STMT.FunctionDef(id, args, body, returnType) =>
        val fnArgs = args.args.map { case EXPR.Declaration(EXPR.Name(n, _, _), Some(t)) =>
          n.name -> Types.typeByIdent(t.name).get
        }.toIndexedSeq
        val retT = Types.typeByIdent(returnType.name).get
        currentCtx = currentCtx.updated(
          ESFunc(id.name, fnArgs, retT, body)
        )
        Left(ESUnit)

      case STMT.If(test, body, orelse) =>
        val nestedCtx = currentCtx.emptyChild(s"if_stmt_${Random.nextInt()}")
        if (eval[Boolean](test)) execute(body, nestedCtx)
        else execute(orelse, nestedCtx)

      case STMT.UnlockIf(test) =>
        if (eval[Boolean](test)) throw UnlockException
        else Left(ESUnit)

      case STMT.Halt => throw ExecAbortException

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
    case Failure(_: UnlockException.type) => Right(Result(Unlocked))
    case Failure(_: ExecAbortException.type) => Right(Result(Halt))
    case Success(Right(out)) => Right(out)
    case Success(Left(_)) => Left(ESUnit)
    case Failure(e) =>
      e.printStackTrace()
      Left(ESUnit)
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
