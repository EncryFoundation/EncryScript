package encrywm.backend.executor

import encrywm.ast.Ast.EXPR.{IntConst, LongConst}
import encrywm.ast.Ast._
import encrywm.backend.env._
import encrywm.backend.executor.error._
import encrywm.backend.{Arith, Compare}
import encrywm.lib.Types
import encrywm.lib.Types.{ESFunc => _, _}
import scorex.crypto.encode.Base58

import scala.util.{Failure, Random, Success, Try}

class Executor(globalEnv: ScopedRuntimeEnv) {

  import Executor._

  private var _globalEnv: ScopedRuntimeEnv = globalEnv

  def executeContract(c: TREE_ROOT.Contract): ExecOutcome = execute(c.body)

  private def execute(statements: Seq[STMT],
                      localEnv: ScopedRuntimeEnv = globalEnv): ExecOutcome = Try {

    var currentEnv = localEnv

    def randCode: Int = Random.nextInt()

    def eval[T](expr: EXPR): T = {
      (expr match {
        case EXPR.Name(id, _, _) =>
          getFromEnv(id.name).map {
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
          getFromEnv(id.name).map {
            case ESFunc(_, fnArgs, _, body) =>
              val argMap = args.zip(fnArgs).map { case (exp, (argN, _)) =>
                val expT = exp.tpeOpt.get
                val expV = eval[expT.Underlying](exp)
                ESValue(argN, expT)(expV)
              }.map(v => v.name -> v).toMap
              val nestedEnv = currentEnv.child(id.name, argMap) // TODO: Handle kwargs.
              execute(body, nestedEnv) match {
                case Right(Return(Val(v))) => v
                case Right(Return(Unlocked)) => throw UnlockException
                case Right(Return(Halt)) => throw ExecAbortException
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
                  val idxV = eval[Int](idx)
                  if (lst.length > idxV) Some(lst(idxV))
                  else None
                case dct: Map[idxT.Underlying@unchecked, _] =>
                  dct.get(eval[idxT.Underlying](idx))
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

        case EXPR.SizeOf(coll) =>
          coll.tpeOpt.get match {
            case ESList(valT) =>
              eval[List[valT.Underlying]](coll).size
            case ESDict(keyT, valT) =>
              eval[Map[keyT.Underlying, valT.Underlying]](coll).size
          }

        case EXPR.Sum(coll, Some(_)) =>
          coll.tpeOpt.get match {
            case ESList(_: ESInt.type) =>
              eval[List[Int]](coll).sum
            case ESList(_: ESLong.type) =>
              eval[List[Long]](coll).sum
          }

        case EXPR.IsDefined(opt) =>
          opt.tpeOpt.get match {
            case ESOption(inT) =>
              eval[Option[inT.Underlying]](opt).isDefined
          }

        case EXPR.Get(opt, Some(_)) =>
          opt.tpeOpt.get match {
            case ESOption(inT) =>
              eval[Option[inT.Underlying]](opt).get
          }

        case EXPR.Exists(coll, EXPR.Lambda(args, body, Some(Types.ESFunc(_: Types.ESBoolean.type)))) =>
          coll.tpeOpt.get match {
            case ESList(tpe) if args.args.size == 1 =>
              val localN = args.args.head._1.name
              val localT = Types.typeByIdent(args.args.head._2.ident.name).get
              if (tpe != localT) throw IllegalOperationError
              def untilTrue: Boolean = {
                for (elt <- eval[List[tpe.Underlying]](coll)) {
                  val localV = ESValue(localN, tpe)(elt)
                  if (applyLambda[Boolean](Map(localN -> localV), body))
                    return true
                }
                false
              }
              untilTrue
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

    def applyLambda[T](argMap: Map[String, ESValue], body: EXPR): T = {
      val nestedEnv = currentEnv.child(s"lambda_$randCode", argMap)
      execute(List(STMT.Return(Some(body))), nestedEnv) match {
        case Right(Return(Val(v: T@unchecked))) => v
        case _ => throw new ExecutionError("Lambda execution error")
      }
    }

    def exec(stmt: STMT): ExecOutcome = stmt match {

      case STMT.Let(EXPR.Declaration(EXPR.Name(id, _, _), _), value, global) =>
        val valT = value.tpeOpt.get
        val esVal = ESValue(id.name, valT)(eval[valT.Underlying](value))
        if (global) {
          _globalEnv = _globalEnv.updated(esVal)
        } else {
          currentEnv = currentEnv.updated(esVal)
        }
        Right(Nothing)

      case STMT.Expr(_) =>
        Right(Nothing)

      case STMT.FunctionDef(id, args, body, returnType) =>
        val fnArgs = args.args.map { case (n, t) =>
          n.name -> Types.typeByIdent(t.ident.name).get
        }.toIndexedSeq
        val retT = Types.typeByIdent(returnType.name).get
        currentEnv = currentEnv.updated(
          ESFunc(id.name, fnArgs, retT, body)
        )
        Right(Nothing)

      case STMT.Match(target, branches) =>
        val targetT = target.tpeOpt.get
        val targetV = eval[targetT.Underlying](target)
        for (branch <- branches) branch match {
          case STMT.Case(_, body, isDefault) if isDefault =>
            val nestedCtx = currentEnv.emptyChild(s"match_stmt_$randCode")
            return execute(body, nestedCtx)
          case STMT.Case(EXPR.BranchParamDeclaration(local, tpeN), body, _) =>
            val localT = Types.typeByIdent(tpeN.ident.name).get
            targetV match {
              case obj: ESObject if obj.isInstanceOf(localT) =>
                val nestedCtx = currentEnv.emptyChild(s"match_stmt_$randCode")
                return execute(body, nestedCtx.updated(ESValue(local.name, localT)(obj.asInstanceOf[localT.Underlying])))
              case _ => // Do nothing.
            }
          case STMT.Case(cond, body, _) =>
            val condT = cond.tpeOpt.get
            val condV = eval[condT.Underlying](cond)
            if (Compare.eq(condV, targetV)) {
              val nestedCtx = currentEnv.emptyChild(s"match_stmt_$randCode")
              return execute(body, nestedCtx)
            }
          case _ => throw IllegalOperationError
        }
        Right(Nothing)

      case STMT.If(test, body, orelse) =>
        val nestedCtx = currentEnv.emptyChild(s"if_stmt_$randCode")
        if (eval[Boolean](test)) execute(body, nestedCtx)
        else execute(orelse, nestedCtx)

      case STMT.UnlockIf(test) =>
        if (eval[Boolean](test)) throw UnlockException
        else Right(Nothing)

      case STMT.Halt => throw ExecAbortException

      case STMT.Pass => Right(Nothing)

      case STMT.Return(None) => Right(Return(Nothing))

      case STMT.Return(Some(v)) =>
        val valT = v.tpeOpt.get
        Right(Return(Val(eval[valT.Underlying](v))))
    }

    def execMany(stmts: Seq[STMT]): ExecOutcome = {
      for (stmt <- stmts) {
        exec(stmt) match {
          case Right(Return(r)) =>
            return Right(Return(r))
          case _ => // Do nothing
        }
      }
      Right(Nothing)
    }

    def getFromEnv(n: String): Option[ESEnvComponent] =
      currentEnv.get(n).orElse(_globalEnv.get(n))

    execMany(statements)
  } match {
    case Failure(_: UnlockException.type) => Right(Return(Unlocked))
    case Failure(_: ExecAbortException.type) => Right(Return(Halt))
    case Success(Right(result)) => Right(result)
    case Failure(e) =>
      e.printStackTrace()
      Left(ExecutionFailed)
  }
}

object Executor {

  type ExecOutcome = Either[ExecutionFailed.type, Result]

  sealed trait Result

  case class Return(r: Any) extends Result

  case object Nothing extends Result

  case class Val(v: Any)

  case object Unlocked

  case object Halt

  case object ExecutionFailed
}
