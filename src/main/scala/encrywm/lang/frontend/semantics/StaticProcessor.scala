package encrywm.lang.frontend.semantics

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.Ast._
import encrywm.lang.frontend.semantics.exceptions._
import encrywm.lang.frontend.semantics.scope._
import encrywm.lib.Types._
import encrywm.lib.{ESMath, TypeSystem}
import encrywm.utils.Stack
import monix.eval.Coeval
import scorex.crypto.encode.Base58

import scala.util.{Random, Try}

class StaticProcessor(ts: TypeSystem) {

  private lazy val scopes: Stack[ScopedSymbolTable] = new Stack

  private def currentScopeOpt: Option[ScopedSymbolTable] = scopes.currentOpt

  def process(contract: Contract): Try[Contract] = {
    val local = contract.copy()
    Try(scan(local)).map(_ => local)
  }

  private def scan(node: AST_NODE): Unit = node match {
    case root: TREE_ROOT => scanRoot(root)
    case stmt: STMT => scanStmt(stmt)
    case expr: EXPR => scanExpr(expr)
    case _ => // Do nothing.
  }

  private def scanRoot(node: TREE_ROOT): Unit = node match {
    case c: TREE_ROOT.Contract =>
      scopes.push(ScopedSymbolTable.initialized)
      c.body.foreach(scan)
    case _ => // Do nothing.
  }

  private def scanStmt(node: STMT): Unit = node match {

    case asg: STMT.Let =>
      scan(asg.value)
      asg.target match {
        case EXPR.Declaration(name: EXPR.Name, typeOpt) =>
          // Infer type for the value being assigned,
          // if the value is func then it's return type is used.
          val valueType = inferType(asg.value) match {
            case ESFunc(_, retT) => retT
            case otherT => otherT
          }
          typeOpt.map { t =>
            val mainT = ts.typeByIdent(t.ident.name).getOrElse(throw NameException(t.ident.name))
            val typeParams = t.typeParams.map(id => ts.typeByIdent(id.name).getOrElse(throw NameException(id.name)))
            mainT -> typeParams
          }.foreach {
            case (ESOption(_), tps) if tps.size == 1 => matchType(ESOption(tps.head), valueType)
            case (ESList(_), tps) if tps.size == 1 => matchType(ESList(tps.head), valueType)
            case (ESDict(_, _), tps) if tps.size == 2 => matchType(ESDict(tps.head, tps.last), valueType)
            case (otherT, tps) if tps.isEmpty => matchType(otherT, valueType)
            case _ => throw TypeException
          }
          if (asg.global) addNameToGlobalScope(name, valueType)
          else addNameToScope(name, valueType)
        case _ => throw IllegalExprException
      }

    case fd: STMT.FunctionDef =>
      val declaredRetType = ts.typeByIdent(fd.returnType.name)
        .getOrElse(throw NameException(fd.returnType.name))
      val params = fd.args.args.map { arg =>
        val argT = ts.typeByIdent(arg._2.ident.name).getOrElse(throw UnresolvedSymbolException(arg._2.ident.name))
        arg._1.name -> argT
      }
      currentScopeOpt.foreach(_.insert(Symbol(fd.name.name, ESFunc(params, declaredRetType))))
      val fnScope = ScopedSymbolTable(fd.name.name, currentScopeOpt.get)
      scopes.push(fnScope)
      params.foreach(p => currentScopeOpt.foreach(_.insert(Symbol(p._1, p._2))))
      fd.body.foreach(scan)

      val retType = findReturns(fd.body).map(_.value.map { exp =>
        exp.tpeOpt.get
      }).foldLeft(Seq[ESType]()) { case (acc, tOpt) =>
        val tpe = tOpt.getOrElse(ESUnit)
        if (acc.nonEmpty) matchType(acc.head, tpe)
        acc :+ tpe
      }.headOption.getOrElse(ESUnit)
      matchType(declaredRetType, retType)

      scopes.popHead()

    case ret: STMT.Return =>
      ret.value.foreach(scanExpr)

    case expr: STMT.Expr =>
      scanExpr(expr.value)

    case ifStmt: STMT.If =>
      scanExpr(ifStmt.test)
      val bodyScope = ScopedSymbolTable(s"if_body_${Random.nextInt()}", currentScopeOpt.get)
      scopes.push(bodyScope)
      ifStmt.body.foreach(scanStmt)
      scopes.popHead()
      val elseScope = ScopedSymbolTable(s"if_else_${Random.nextInt()}", currentScopeOpt.get)
      scopes.push(elseScope)
      ifStmt.orelse.foreach(scanStmt)
      scopes.popHead()

    case STMT.Match(target, branches) =>
      scanExpr(target)
      if (!branches.forall(_.isInstanceOf[STMT.Case]))
        throw UnexpectedStatementException("Case clause is expected")
      else if (!branches.last.asInstanceOf[STMT.Case].isDefault)
        throw DefaultBranchUndefinedException
      branches.foreach(scanStmt)

    case STMT.Case(cond, body, _) =>
      scanExpr(cond)
      val bodyScope = ScopedSymbolTable(s"case_branch_${Random.nextInt()}", currentScopeOpt.get)
      scopes.push(bodyScope)
      cond match {
        case EXPR.TypeMatching(local, tpe) =>
          val localT = ts.typeByIdent(tpe.ident.name).getOrElse(throw TypeException)
          currentScopeOpt.foreach(_.insert(Symbol(local.name, localT)))
        case EXPR.SchemaMatching(local, Identifier(schemaId)) =>
          val localT = ts.typeByIdent(schemaId).getOrElse(throw TypeException)
          currentScopeOpt.foreach(_.insert(Symbol(local.name, localT)))
        case _ => // Do nothing.
      }
      body.foreach(scanStmt)
      scopes.popHead()

    case STMT.UnlockIf(test) =>
      scanExpr(test)

    case _ => // Do nothing.
  }

  private def scanExpr(node: EXPR): Unit = {
    def scanMany(exprs: Seq[EXPR]): Unit = exprs.foreach(scanExpr)
    node match {
      case n: EXPR.Name =>
        assertDefined(n.id.name)

      case bo: EXPR.BoolOp =>
        bo.values.foreach(scanExpr)

      case bin: EXPR.BinOp =>
        Seq(bin.left, bin.right).foreach(scanExpr)

      case EXPR.Lambda(args, body, _) =>
        val paramSymbols = args.args.map { arg =>
          val argT = ts.typeByIdent(arg._2.ident.name).getOrElse(throw UnresolvedSymbolException(arg._2.ident.name))
          Symbol(arg._1.name, argT)
        }
        val bodyScope = ScopedSymbolTable(s"lamb_body_${Random.nextInt()}", currentScopeOpt.get)
        scopes.push(bodyScope)
        paramSymbols.foreach(s => currentScopeOpt.foreach(_.insert(s)))
        scanExpr(body)
        scopes.popHead()

      case EXPR.Call(EXPR.Name(id, _, _), args, keywords, _) =>
        currentScopeOpt.flatMap(_.lookup(id.name)).map { case Symbol(_, ESFunc(params, _)) =>
          if (params.size != args.size + keywords.size) throw WrongNumberOfArgumentsException(id.name)
          val argTypes = params.map(_._2)
          args.map(inferType).zip(argTypes).foreach { case (t1, t2) =>
            matchType(t1, t2)
          }
          id.name
        }.getOrElse(throw NameException(id.name))
        args.foreach(scanExpr)
        keywords.map(_.value).foreach(scanExpr)

      case EXPR.Call(func: EXPR.Attribute, args, keywords, _) =>
        scanMany(Seq(func, func.value) ++ args ++ keywords.map(_.value))
        func.value.tpeOpt.get match {
          case coll: ESCollection if args.size == 1 =>
            coll.getAttrType(func.attr.name) match {
              case Some(f: ESFunc) =>
                args.map(inferType).zip(f.args.map(_._2)).foreach { case (t1, t2) =>
                  matchType(t1, t2)
                }
            }
          case _ =>
            currentScopeOpt.flatMap(_.lookup(func.attr.name)).map { case Symbol(_, ESFunc(params, _)) =>
              if (params.size != args.size + keywords.size) throw WrongNumberOfArgumentsException(func.attr.name)
              val argTypes = params.map(_._2)
              args.map(inferType).zip(argTypes).foreach { case (t1, t2) =>
                matchType(t1, t2)
              }
              func.attr.name
            }.getOrElse(throw NameException(func.attr.name))
        }

      case cmp: EXPR.Compare =>
        cmp.comparators.foreach(scanExpr)
        scanExpr(cmp.left)

      case uop: EXPR.UnaryOp => scanExpr(uop.operand)

      case ifExp: EXPR.IfExp =>
        Seq(ifExp.test, ifExp.body, ifExp.orelse).foreach(scanExpr)

      case dct: EXPR.ESDictNode =>
        dct.keys.foreach(scanExpr)
        if (!dct.keys.forall(k => k.tpeOpt.get.isPrimitive)) throw IllegalExprException
        dct.values.foreach(scanExpr)

      case lst: EXPR.ESList => lst.elts.foreach(scanExpr)

      case sub: EXPR.Subscript =>
        scanExpr(sub.value)
        sub.slice match {
          case SLICE.Index(idx) =>
            scanExpr(idx)
            val idxT = idx.tpeOpt.get
            sub.value.tpeOpt match {
              case Some(ESList(_)) => matchType(idxT, ESInt)
              case Some(ESDict(keyT, _)) => matchType(idxT, keyT)
              case _ => throw IllegalExprException
            }
          // TODO: Complete for other SLICE_OPs.
        }

      case EXPR.TypeMatching(_, tpe) =>
        ts.typeByIdent(tpe.ident.name).getOrElse(throw UnresolvedSymbolException(tpe.ident.name))

      case EXPR.Base58Str(s) =>
        if (Base58.decode(s).isFailure) throw Base58DecodeException

      case _ => // Do nothing.
    }
    inferType(node)
  }

  private def addNameToScope(name: EXPR.Name, tpe: ESType): Unit = {
    currentScopeOpt.foreach(_.insert(Symbol(name.id.name, tpe)))
  }

  private def addNameToGlobalScope(name: EXPR.Name, tpe: ESType): Unit = {
    scopes.lastOpt.foreach(_.insert(Symbol(name.id.name, tpe)))
  }

  private def findReturns(stmts: Seq[STMT]): Seq[STMT.Return] = {

    def findReturnsIn(stmt: STMT): Seq[STMT.Return] = stmt match {
      case ret: STMT.Return => Seq(ret)
      case STMT.If(_, body, orelse) => findReturns(body) ++ findReturns(orelse)
      case STMT.Match(_, branches) => findReturns(branches)
      case STMT.Case(_, body, _) => findReturns(body)
      case STMT.FunctionDef(_, _, body, _) => findReturns(body)
      case _ => Seq.empty
    }

    stmts.flatMap(findReturnsIn)
  }

  private def inferType(exp: EXPR): ESType = {

    val scope = currentScopeOpt.getOrElse(throw MissedContextException)

    def inferTypeIn(e: EXPR): ESType = e.tpeOpt.getOrElse {
      exp match {
        case n: EXPR.Name => scope.lookup(n.id.name)
          .map(_.tpe).getOrElse(throw NameException(n.id.name))

        case attr: EXPR.Attribute =>
          inferType(attr.value) match {
            case p: ESProduct =>
              p.getAttrType(attr.attr.name).getOrElse(throw NameException(attr.attr.name))
            case ESFunc(_, retT) => retT
            case _ => throw IllegalExprException
          }

        case fc: EXPR.Call =>
          fc.args.map(inferType)
          fc.func match {
            case EXPR.Name(n, _, _) =>
              scope.lookup(n.name).map { case Symbol(_, t) => t }
                .getOrElse(throw IllegalExprException)

            // Special handler for `.map()`
            case EXPR.Attribute(value, n, _, _)
              if n.name == "map" && fc.args.size == 1 =>
              inferType(value) match {
                case coll: ESCollection =>
                  coll.getAttrType(n.name) match {
                    case Some(_: ESFunc) =>
                      inferType(fc.args.head) match {
                        case ESFunc(_, retT) => ESList(retT)
                      }
                    case _ => throw IllegalExprException
                  }
                case _ => throw IllegalExprException
              }

            case EXPR.Attribute(value, n, _, _) =>
              inferType(value) match {
                case pt: ESProduct =>
                  pt.getAttrType(n.name) match {
                    case Some(funcT: ESFunc) => funcT
                    case _ => throw IllegalExprException
                  }
                case _ => throw IllegalExprException
              }

            case _ => throw IllegalExprException
          }

        case bop: EXPR.BinOp =>
          ESMath.ensureZeroDivision(bop.op, bop.right)
          ESMath.BinaryOperationRuleset.find {
            case (op, (o1, o2), _) =>
              bop.op == op && o1 == inferType(bop.left) && o2 == inferType(bop.right)
          }.map(_._3).getOrElse(throw IllegalOperandException)

        case ifExp: EXPR.IfExp =>
          val bodyType = inferType(ifExp.body)
          val elseType = inferType(ifExp.orelse)
          if (bodyType != elseType) throw IllegalExprException
          bodyType

        case uop: EXPR.UnaryOp => inferType(uop.operand)

        case EXPR.ESList(elts, _, _) =>
          val listT = elts.headOption.map(inferType)
            .getOrElse(throw IllegalExprException)
          elts.tail.foreach(e => matchType(listT, inferType(e)))
          ensureNestedColl(elts)
          ESList(listT)

        case EXPR.ESDictNode(keys, vals, _) =>
          val keyT = keys.headOption.map(inferType).getOrElse(ESUnit)
          val valT = vals.headOption.map(inferType).getOrElse(ESUnit)
          keys.tail.foreach(k => matchType(keyT, inferType(k)))
          vals.tail.foreach(v => matchType(valT, inferType(v)))
          ensureNestedColl(vals)
          ESDict(keyT, valT)

        case EXPR.Subscript(value, SLICE.Index(_), _, _) =>
          inferType(value) match {
            case list: ESList => ESOption(list.valT)
            case dict: ESDict => ESOption(dict.valT)
          }

        case EXPR.Lambda(args, body, _) =>
          ESFunc(args.args.map { case (argId, typeId) =>
            argId.name -> ts.typeByIdent(typeId.ident).get }, inferType(body))

        case _ => throw IllegalExprException
      }
    }

    val tpe = inferTypeIn(exp)
    if (exp.tpeOpt.isEmpty) exp.tpeOpt = Some(tpe)
    tpe
  }

  private def ensureNestedColl(exps: Seq[EXPR]): Unit = exps.foreach { exp =>
    val expT = exp.tpeOpt.get
    if (expT.isInstanceOf[ESList] || expT.isInstanceOf[ESDict]) throw NestedCollectionException
  }

  private def matchType(t1: ESType, t2: ESType): Unit =
    if (!(t1 == t2 || t2.isSubtypeOf(t1))) throw TypeMismatchException(t1.ident, t2.ident)

  private def assertDefined(n: String): Unit = if (currentScopeOpt.flatMap(_.lookup(n)).isEmpty) throw NameException(n)
}

object StaticProcessor {

  def default: StaticProcessor = new StaticProcessor(TypeSystem.default)
}
