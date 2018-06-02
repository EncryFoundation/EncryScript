package encrywm.lang.backend.executor

import encrywm.ast.Ast.EXPR.{IntConst, Lambda, LongConst, Name}
import encrywm.ast.Ast._
import encrywm.lang.backend.env._
import encrywm.lang.backend.executor.error._
import encrywm.lang.backend.{Arith, Compare}
import encrywm.lib.Types.{ESFunc => _, _}
import encrywm.lib.predef.PredefFunctions
import encrywm.lib.{TypeSystem, Types}
import monix.eval.Coeval
import scorex.crypto.encode.Base58

import scala.util.{Failure, Random, Success}

class Executor private[encrywm](scopedRuntimeEnv: ScopedRuntimeEnv,
                                ts: TypeSystem = TypeSystem.default,
                                fuelLimit: Int = 1000,
                                debug: Boolean = false) {
  import Executor._

  private var globalEnv: ScopedRuntimeEnv = scopedRuntimeEnv

  private var stepsCount: Int = 0

  def executeContract(c: TREE_ROOT.Contract): ExecOutcome = execute(c.body)

  private def execute(statements: Seq[STMT],
                      localEnv: ScopedRuntimeEnv = scopedRuntimeEnv): ExecOutcome = Coeval {

    var currentEnv: ScopedRuntimeEnv = localEnv

    def randCode: Int = Random.nextInt()

    def eval[T](expr: EXPR): T = {
      if (stepsCount > fuelLimit) {
        throw IllegalOperationException
      } else stepsCount += 1
      (expr match {
        case EXPR.Name(id, _) =>
          getFromEnv(id.name).map {
            case v: ESValue => v.value
            case o: ESObject => o
            case _: ESFunc => throw IsFunctionException(id.name)
          }.getOrElse(throw UnresolvedReferenceException(id.name))

        case EXPR.BinOp(l, op, r, tpeOpt) =>
          val opT: ESType = tpeOpt
          val leftT: ESType = l.tipe
          val rightT: ESType = r.tipe
          val leftV: leftT.Underlying = eval[leftT.Underlying](l)
          val rightV: rightT.Underlying = eval[rightT.Underlying](r)
          op match {
            case _: OPERATOR.Add.type =>
              Arith.add[opT.Underlying](leftV, rightV)
            case _: OPERATOR.Sub.type =>
              Arith.sub[opT.Underlying](leftV, rightV)
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
          val leftT: ESType = left.tipe
          val leftV: leftT.Underlying = eval[leftT.Underlying](left)
          ops.zip(comps).forall {
            case (COMP_OP.Eq, comp) =>
              val compT: ESType = comp.tipe
              Compare.eq(leftV, eval[compT.Underlying](comp))
            case (COMP_OP.Gt, comp) =>
              val compT: ESType = comp.tipe
              Compare.gt(leftV, eval[compT.Underlying](comp))
            case (COMP_OP.GtE, comp) =>
              val compT: ESType = comp.tipe
              Compare.gte(leftV, eval[compT.Underlying](comp))
            case (COMP_OP.Lt, comp) =>
              val compT: ESType = comp.tipe
              Compare.lt(leftV, eval[compT.Underlying](comp))
            case (COMP_OP.LtE, comp) =>
              val compT: ESType = comp.tipe
              Compare.lte(leftV, eval[compT.Underlying](comp))
          }

        // TODO: `kwargs` handling?
        case EXPR.Call(EXPR.Name(id, _), args, _, _) =>
          getFromEnv(id.name).map {
            case ESFunc(_, fnArgs, _, body) =>
              val argMap: Map[String, ESValue] = args.zip(fnArgs).map { case (exp, (argN, _)) =>
                val expT: ESType = exp.tipe
                val expV: expT.Underlying = eval[expT.Underlying](exp)
                ESValue(argN, expT)(expV)
              }.map(v => v.id -> v).toMap
              val nestedEnv: ScopedRuntimeEnv = currentEnv.child(id.name, argMap, isFunc = true)
              execute(body, nestedEnv) match {
                case Right(Return(Val(v))) => v
                case Right(Return(Unlocked)) => throw UnlockException
                case Right(Return(Halt)) => throw ExecAbortException
                case _ => // Do nothing.
              }

            case ESBuiltInFunc(name, dArgs, body) =>
              if (PredefFunctions.hashFunctions.map(_.name).contains(name))
                stepsCount += 9
              if (PredefFunctions.heavyFunctions.map(_.name).contains(name))
                stepsCount += 29
              val fnArgs: List[(String, ESValue)] = args.zip(dArgs).map { case (arg, (n, _)) =>
                val argT: ESType = arg.tipe
                val argV: argT.Underlying = eval[argT.Underlying](arg)
                n -> ESValue(n, argT)(argV)
              }
              body(fnArgs) match {
                case Right(r) => r
                case _ => throw BuiltInFunctionExecException
              }

            case other => throw NotAFunctionException(other.toString)
          }.getOrElse(throw UnresolvedReferenceException(id.name))

        case EXPR.Attribute(value, attr, _) =>
          val valT: ESType = value.tipe
          eval[valT.Underlying](value) match {
            case obj: ESObject => obj.getAttr(attr.name).get.value
            case _ => throw IllegalOperationException
          }

        case EXPR.IfExp(test, body, orelse, tpe) =>
          if (eval[Boolean](test)) {
            eval[tpe.Underlying](body)
          } else {
            eval[tpe.Underlying](orelse)
          }

        case EXPR.UnaryOp(op, operand, _) =>
          op match {
            case UNARY_OP.Not => !eval[Boolean](operand)
            case UNARY_OP.Invert => operand match {
              case exp: IntConst => eval[Int](exp) * (-1)
              case exp: LongConst => eval[Long](exp) * (-1)
            }
            case _ => throw IllegalOperationException
          }

        case EXPR.Subscript(exp, slice, _) =>
          val expT: ESType = exp.tipe
          slice match {
            case SLICE.Index(idx) =>
              val idxT: ESType = idx.tipe
              eval[expT.Underlying](exp) match {
                case lst: List[idxT.Underlying@unchecked] =>
                  val idxV: Int = eval[Int](idx)
                  if (lst.length > idxV) Some(lst(idxV))
                  else None
                case dct: Map[idxT.Underlying@unchecked, _] =>
                  dct.get(eval[idxT.Underlying](idx))
                case _ => throw IllegalOperationException
              }

            case _ => throw IllegalOperationException
          }

        case EXPR.ESList(elts, ESList(valT)) =>
          if (elts.size > 50) throw IllegalOperationException
          elts.foldLeft(List[valT.Underlying]()) { case (acc, exp) =>
            acc :+ eval[valT.Underlying](exp)
          }

        case EXPR.ESDictNode(keys, values, ESDict(keyT, valT)) =>
          if (keys.size > 50) throw IllegalOperationException
          keys.zip(values).foldLeft(Map[keyT.Underlying, valT.Underlying]()) { case (acc, (k, v)) =>
            acc.updated(eval[keyT.Underlying](k), eval[valT.Underlying](v))
          }

        case EXPR.SizeOf(coll) =>
          coll.tipe match {
            case ESList(valT) =>
              eval[List[valT.Underlying]](coll).size
            case ESDict(keyT, valT) =>
              eval[Map[keyT.Underlying, valT.Underlying]](coll).size
          }

        case EXPR.Sum(coll, _) =>
          coll.tipe match {
            case ESList(_: ESInt.type) =>
              eval[List[Int]](coll).sum
            case ESList(_: ESLong.type) =>
              eval[List[Long]](coll).sum
          }

        case EXPR.IsDefined(opt) =>
          opt.tipe match {
            case ESOption(inT) =>
              eval[Option[inT.Underlying]](opt).isDefined
          }

        case EXPR.Get(opt, _) =>
          opt.tipe match {
            case ESOption(inT) =>
              eval[Option[inT.Underlying]](opt).get
            case Types.ESFunc(_, ESOption(inT)) =>
              eval[Option[inT.Underlying]](opt).get
          }

        case EXPR.Map(coll, func, ESList(inT)) =>
          func.tipe match {
            case Types.ESFunc(args, _) =>
              coll.tipe match {
                case ESList(valT) if args.size == 1 =>
                  val localN: String = args.head._1
                  eval[List[valT.Underlying]](coll).map { elt =>
                    val localV = ESValue(localN, valT)(elt)
                    func match {
                      case lamb: Lambda =>
                        applyLambda[inT.Underlying](Map(localN -> localV), lamb.body)
                      case Name(id, _) => getFromEnv(id.name).map {
                        case fn: ESFunc =>
                          applyFunc[inT.Underlying](Map(localN -> localV), fn.body)
                      }.get
                    }
                  }
                case ESDict(keyT, valT) if args.size == 2 =>
                  val keyN: String = args.head._1
                  val valN: String = args.last._1
                  eval[Map[keyT.Underlying, valT.Underlying]](coll).map { case (k, v) =>
                    val keyV: ESValue = ESValue(keyN, keyT)(k)
                    val valV: ESValue = ESValue(valN, valT)(v)
                    func match {
                      case lamb: Lambda =>
                        applyLambda[inT.Underlying](Map(keyN -> keyV, valN -> valV), lamb.body)
                      case Name(id, _) => getFromEnv(id.name).map {
                        case fn: ESFunc =>
                          applyFunc[inT.Underlying](Map(keyN -> keyV, valN -> valV), fn.body)
                      }.get
                    }
                  }
              }
            case _ => throw IllegalOperationException
          }

        case EXPR.Exists(coll, predicate) =>
          predicate.tipe match {
            case Types.ESFunc(args, _) =>
              coll.tipe match {
                case ESList(tpe) if args.size == 1 =>
                  val localN: String = args.head._1
                  def untilTrue: Boolean = {
                    for (elt <- eval[List[tpe.Underlying]](coll)) {
                      val localV: ESValue = ESValue(localN, tpe)(elt)
                      predicate match {
                        case lamb: Lambda =>
                          if (applyLambda[Boolean](Map(localN -> localV), lamb.body))
                            return true
                        case Name(id, _) => getFromEnv(id.name).foreach {
                          case fn: ESFunc =>
                            if (applyFunc[Boolean](Map(localN -> localV), fn.body))
                              return true
                        }
                      }
                    }
                    false
                  }
                  untilTrue
              }
            case _ => throw IllegalOperationException
          }

        case EXPR.Base58Str(s) => Base58.decode(s).get

        case EXPR.Str(s) => s

        case EXPR.True => true

        case EXPR.False => false

        case EXPR.IntConst(v) => v

        case EXPR.LongConst(v) => v

        case exp => throw UnexpectedExpressionException(exp.toString)
      }).asInstanceOf[T]
    }

    def applyLambda[T](argMap: Map[String, ESValue], body: EXPR): T = {
      val nestedEnv: ScopedRuntimeEnv = currentEnv.child(s"lambda_$randCode", argMap)
      execute(List(STMT.Return(Some(body))), nestedEnv) match {
        case Right(Return(Val(v: T@unchecked))) if v.isInstanceOf[T] => v
        case _ => throw FunctionExecException
      }
    }

    def applyFunc[T](argMap: Map[String, ESValue], body: Seq[STMT]): T = {
      val nestedEnv: ScopedRuntimeEnv = currentEnv.child(s"fn_$randCode", argMap)
      execute(body, nestedEnv) match {
        case Right(Return(Val(v: T@unchecked))) if v.isInstanceOf[T] => v
        case _ => throw FunctionExecException
      }
    }

    def exec(stmt: STMT): ExecOutcome = stmt match {

      case STMT.Let(EXPR.Declaration(EXPR.Name(id, _), _), value) =>
        val valT: ESType = value.tipe
        val esVal: ESValue = ESValue(id.name, valT)(eval[valT.Underlying](value))
        currentEnv = currentEnv.updated(esVal)
        Right(Nothing)

      case STMT.Expr(_) =>
        Right(Nothing)

      case STMT.FunctionDef(id, args, body, returnType) =>
        val fnArgs: IndexedSeq[(String, ESType)] = args.args.map { case (n, t) =>
          n.name -> ts.typeByIdent(t.ident.name).get
        }.toIndexedSeq
        val retT: ESType = ts.typeByIdent(returnType.name).get
        currentEnv = currentEnv.updated(
          ESFunc(id.name, fnArgs, retT, body)
        )
        Right(Nothing)

      case STMT.Match(target, branches) =>
        val targetT: ESType = target.tipe
        val targetV: targetT.Underlying = eval[targetT.Underlying](target)
        for (branch <- branches) branch match {
          case STMT.Case(_, body, isDefault) if isDefault =>
            val nestedCtx: ScopedRuntimeEnv = currentEnv.emptyChild(s"match_stmt_$randCode")
            return execute(body, nestedCtx)

          case STMT.Case(EXPR.SchemaMatching(local, Identifier(schemaId)), body, _) =>
            targetV match {
              case obj: ESObject if obj.isInstanceOf(Types.SDObject) && obj.id == schemaId =>
                val nestedCtx: ScopedRuntimeEnv = currentEnv.emptyChild(s"match_stmt_$randCode")
                return execute(body, nestedCtx.updated(ESValue(local.name, Types.SDObject)(obj.asInstanceOf[Types.SDObject.Underlying])))
            }

          case STMT.Case(EXPR.TypeMatching(local, tpeN), body, _) =>
            val localT: ESType = ts.typeByIdent(tpeN.ident.name).get
            targetV match {
              case obj: ESObject if obj.isInstanceOf(localT) =>
                val nestedCtx: ScopedRuntimeEnv = currentEnv.emptyChild(s"match_stmt_$randCode")
                return execute(body, nestedCtx.updated(ESValue(local.name, localT)(obj.asInstanceOf[localT.Underlying])))
              case _ => // Do nothing.
            }

          case STMT.Case(cond, body, _) =>
            val condT = cond.tipe
            val condV = eval[condT.Underlying](cond)
            if (Compare.eq(condV, targetV)) {
              val nestedCtx = currentEnv.emptyChild(s"match_stmt_$randCode")
              return execute(body, nestedCtx)
            }

          case _ => throw IllegalOperationException
        }
        Right(Nothing)

      case STMT.If(test, body, orelse) =>
        val nestedCtx = currentEnv.emptyChild(s"if_stmt_$randCode")
        if (eval[Boolean](test)) execute(body, nestedCtx)
        else execute(orelse, nestedCtx)

      case STMT.UnlockIf(test) =>
        // Disable unlocking inside functions.
        if (currentEnv.isFunc) throw new RuntimeException("'Unlock if' statement appeared within function scope.")
        if (eval[Boolean](test)) throw UnlockException
        else Right(Nothing)

      case STMT.Halt => throw ExecAbortException

      case STMT.Pass => Right(Nothing)

      case STMT.Return(None) => Right(Return(Nothing))

      case STMT.Return(Some(v)) =>
        val valT: ESType = v.tipe
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
      currentEnv.get(n).orElse(globalEnv.get(n))

    execMany(statements)
  }.runTry match {
    case Failure(_: UnlockException.type) => Right(Return(Unlocked))
    case Failure(_: ExecAbortException.type) => Right(Return(Halt))
    case Success(Right(result)) => Right(result)
    case Failure(e) =>
      if (debug) e.printStackTrace()
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

  def apply(ctx: ESValue,
            fuelLimit: Int,
            ts: TypeSystem = TypeSystem.default,
            debug: Boolean = false): Executor = {
    ESContext.fields.foreach { case (name, tpe) =>
      if (!ctx.value.asInstanceOf[ESObject].fields.exists(ctxElem => ctxElem._1 == name && (ctxElem._2.tpe == tpe || ctxElem._2.tpe.isSubtypeOf(tpe))))
        throw new EnvironmentError(s"Environment is inconsistent, $name[$tpe] is undefined.")
    }
    new Executor(ScopedRuntimeEnv.initialized("G", 1, Map(ESContext.ident.toLowerCase -> ctx)), ts, fuelLimit, debug)
  }
}
