package encrywm.lang.frontend.semantics

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.Ast._
import encrywm.ast.AstStringifier
import encrywm.lang.frontend.semantics.exceptions._
import encrywm.lang.frontend.semantics.scope._
import encrywm.lib.Types._
import encrywm.lib.{ESMath, TypeSystem}
import scorex.crypto.encode.Base58

import scala.util.{Random, Try}

class StaticProcessor(ts: TypeSystem) {

  private var scopes: List[ScopedSymbolTable] = List.empty

  private def currentScopeOpt: Option[ScopedSymbolTable] = scopes.headOption

  def process(contract: Contract): Try[Contract] = Try(scan(contract)).map(_ => contract)

  private def scan(node: AST_NODE): Unit = node match {
    case root: TREE_ROOT => scanRoot(root)
    case stmt: STMT => scanStmt(stmt)
    case expr: EXPR => scanExpr(expr)
    case _ => // Do nothing.
  }

  private def scanRoot(node: TREE_ROOT): Unit = node match {
    case c: TREE_ROOT.Contract =>
      scopes = ScopedSymbolTable.initialized :: scopes
      c.body.foreach(scan)
    case _ => // Do nothing.
  }

  private def scanStmt(node: STMT): Unit = node match {

    case asg: STMT.Let =>
      scan(asg.value)
      asg.target match {
        case EXPR.Declaration(name: EXPR.Name, typeOpt) =>
          // Infer type for the value being assigned,
          // if the value is func then its return type is used.
          val valueType: ESType = inferType(asg.value) match {
            case ESFunc(_, retT) => retT
            case otherT => otherT
          }
          typeOpt.map { t =>
            val mainT: ESType = ts.typeByIdent(t.ident.name)
              .getOrElse(throw NameException(t.ident.name, AstStringifier.toString(asg)))
            val typeParams: List[ESType] = t.typeParams.map(id => ts.typeByIdent(id.name)
              .getOrElse(throw NameException(t.ident.name, AstStringifier.toString(asg))))
            mainT -> typeParams
          }.foreach {
            case (ESOption(_), tps) if tps.size == 1 => matchType(ESOption(tps.head), valueType, asg)
            case (ESList(_), tps) if tps.size == 1 => matchType(ESList(tps.head), valueType, asg)
            case (ESDict(_, _), tps) if tps.size == 2 => matchType(ESDict(tps.head, tps.last), valueType, asg)
            case (otherT, tps) if tps.isEmpty => matchType(otherT, valueType, asg)
            case _ => throw TypeException(AstStringifier.toString(asg))
          }
          if (asg.global) addNameToGlobalScope(name, valueType)
          else addNameToScope(name, valueType)
        case _ => throw IllegalExprException(AstStringifier.toString(asg))
      }

    case funcDef: STMT.FunctionDef =>
      val declaredRetType: ESType = ts.typeByIdent(funcDef.returnType.name)
        .getOrElse(throw NameException(funcDef.returnType.name, AstStringifier.toString(funcDef)))
      val params: List[(String, ESType)] = funcDef.args.args.map { arg =>
        val argT = ts.typeByIdent(arg._2.ident.name)
          .getOrElse(throw UnresolvedSymbolException(arg._2.ident.name, AstStringifier.toString(funcDef)))
        arg._1.name -> argT
      }
      currentScopeOpt.foreach(_.insert(Symbol(funcDef.name.name, ESFunc(params, declaredRetType)), node))
      val fnScope: ScopedSymbolTable = ScopedSymbolTable(funcDef.name.name, currentScopeOpt.get, isFunc = true)
      scopes = fnScope :: scopes
      params.foreach(p => currentScopeOpt.foreach(_.insert(Symbol(p._1, p._2), node)))
      funcDef.body.foreach(scan)

      val retType: ESType = findReturnTypes(funcDef.body).foldLeft(Seq[ESType]()) { case (acc, tpe) =>
        if (acc.nonEmpty) matchType(acc.head, tpe, funcDef)
        acc :+ tpe
      }.headOption.getOrElse(ESUnit)
      matchType(declaredRetType, retType, funcDef)

      scopes = scopes.drop(1)

    case STMT.Return(value) =>
      value.foreach(scanExpr)

    case STMT.Expr(value) =>
      scanExpr(value)

    case ifStmt: STMT.If =>
      scanExpr(ifStmt.test)
      val bodyScope = ScopedSymbolTable(s"if_body_${Random.nextInt()}", currentScopeOpt.get)
      scopes = bodyScope :: scopes
      ifStmt.body.foreach(scanStmt)
      scopes = scopes.drop(1)
      val elseScope = ScopedSymbolTable(s"if_else_${Random.nextInt()}", currentScopeOpt.get)
      scopes = elseScope :: scopes
      ifStmt.orelse.foreach(scanStmt)
      scopes = scopes.drop(1)

    case STMT.Match(target, branches) =>
      scanExpr(target)
      if (!branches.forall(_.isInstanceOf[STMT.Case]))
        throw UnexpectedStatementException("Case clause is expected", branches
          .foldLeft("")((str, branch) => str.concat(AstStringifier.toString(branch))))
      else if (!branches.last.asInstanceOf[STMT.Case].isDefault)
        throw DefaultBranchUndefinedException(AstStringifier.toString(branches.last))
      branches.foreach(scanStmt)

    case STMT.Case(cond, body, _) =>
      scanExpr(cond)
      val bodyScope: ScopedSymbolTable = ScopedSymbolTable(s"case_branch_${Random.nextInt()}", currentScopeOpt.get)
      scopes = bodyScope :: scopes
      cond match {
        case EXPR.TypeMatching(local, tpe) =>
          val localT: ESType = ts.typeByIdent(tpe.ident.name)
            .getOrElse(throw TypeException(AstStringifier.toString(cond)))
          currentScopeOpt.foreach(_.insert(Symbol(local.name, localT), node))
        case EXPR.SchemaMatching(local, Identifier(schemaId)) =>
          val localT: ESType = ts.typeByIdent(schemaId)
            .getOrElse(throw TypeException(AstStringifier.toString(cond)))
          currentScopeOpt.foreach(_.insert(Symbol(local.name, localT), node))
        case _ => // Do nothing.
      }
      body.foreach(scanStmt)
      scopes = scopes.drop(1)

    case ui @ STMT.UnlockIf(test) =>
      if (currentScopeOpt.exists(_.isFunc))
        throw IllegalUnlockIfScopeException(AstStringifier.toString(ui))
      scanExpr(test)

    case _ => // Do nothing.
  }

  private def scanExpr(node: EXPR): Unit = {
    def scanMany(exprs: Seq[EXPR]): Unit = exprs.foreach(scanExpr)
    node match {
      case n: EXPR.Name =>
        assertDefined(n.id.name, node)

      case bo: EXPR.BoolOp =>
        bo.values.foreach(scanExpr)

      case bin: EXPR.BinOp =>
        Seq(bin.left, bin.right).foreach(scanExpr)

      case EXPR.Lambda(args, body, _) =>
        val paramSymbols: Seq[Symbol] = args.args.map { arg =>
          val argT: ESType = ts.typeByIdent(arg._2.ident.name)
            .getOrElse(throw UnresolvedSymbolException(arg._2.ident.name, AstStringifier.toString(node)))
          Symbol(arg._1.name, argT)
        }
        val bodyScope: ScopedSymbolTable = ScopedSymbolTable(s"lamb_body_${Random.nextInt()}", currentScopeOpt.get)
        scopes = bodyScope :: scopes
        paramSymbols.foreach(s => currentScopeOpt.foreach(_.insert(s, node)))
        scanExpr(body)
        scopes = scopes.drop(1)

      case EXPR.Call(EXPR.Name(id, _, _), args, keywords, _) =>
        currentScopeOpt.flatMap(_.lookup(id.name)).map { case Symbol(_, ESFunc(params, _)) =>
          if (params.size != args.size + keywords.size)
            throw WrongNumberOfArgumentsException(id.name, AstStringifier.toString(node))
          val argTypes: Seq[ESType] = params.map(_._2)
          args.map(inferType).zip(argTypes).foreach { case (t1, t2) =>
            matchType(t1, t2, node)
          }
          id.name
        }.getOrElse(throw NameException(id.name, AstStringifier.toString(node)))
        args.foreach(scanExpr)
        keywords.map(_.value).foreach(scanExpr)

      case EXPR.Call(func: EXPR.Attribute, args, keywords, _) =>
        scanMany(Seq(func, func.value) ++ args ++ keywords.map(_.value))
        func.value.tpeOpt.get match {
          case coll: ESCollection if args.size == 1 =>
            coll.getAttrType(func.attr.name) match {
              case Some(f: ESFunc) =>
                args.map(inferType).zip(f.args.map(_._2)).foreach { case (t1, t2) =>
                  matchType(t1, t2, node)
                }
            }
          case _ =>
            currentScopeOpt.flatMap(_.lookup(func.attr.name)).map { case Symbol(_, ESFunc(params, _)) =>
              if (params.size != args.size + keywords.size)
                throw WrongNumberOfArgumentsException(func.attr.name, AstStringifier.toString(node))
              val argTypes: Seq[ESType] = params.map(_._2)
              args.map(inferType).zip(argTypes).foreach { case (t1, t2) =>
                matchType(t1, t2, node)
              }
              func.attr.name
            }.getOrElse(throw NameException(func.attr.name, AstStringifier.toString(node)))
        }

      case cmp: EXPR.Compare =>
        cmp.comparators.foreach(scanExpr)
        scanExpr(cmp.left)

      case uop: EXPR.UnaryOp => scanExpr(uop.operand)

      case ifExp: EXPR.IfExp =>
        Seq(ifExp.test, ifExp.body, ifExp.orelse).foreach(scanExpr)

      case dct: EXPR.ESDictNode =>
        dct.keys.foreach(scanExpr)
        if (!dct.keys.forall(k => k.tpeOpt.get.isPrimitive)) throw IllegalExprException(AstStringifier.toString(node))
        dct.values.foreach(scanExpr)

      case lst: EXPR.ESList => lst.elts.foreach(scanExpr)

      case sub: EXPR.Subscript =>
        scanExpr(sub.value)
        sub.slice match {
          case SLICE.Index(idx) =>
            scanExpr(idx)
            val idxT: ESType = idx.tpeOpt.get
            sub.value.tpeOpt match {
              case Some(ESList(_)) => matchType(idxT, ESInt, sub)
              case Some(ESDict(keyT, _)) => matchType(idxT, keyT, sub)
              case _ => throw IllegalExprException(AstStringifier.toString(sub))
            }
          // TODO: Support for other SLICE_OPs.
        }

      case EXPR.TypeMatching(_, tpe) =>
        ts.typeByIdent(tpe.ident.name)
          .getOrElse(throw UnresolvedSymbolException(tpe.ident.name, AstStringifier.toString(node)))

      case EXPR.Base58Str(s) =>
        if (Base58.decode(s).isFailure) throw Base58DecodeException(AstStringifier.toString(node))

      case _ => // Do nothing.
    }
    inferType(node)
  }

  private def addNameToScope(name: EXPR.Name, tpe: ESType): Unit = {
    currentScopeOpt.foreach(_.insert(Symbol(name.id.name, tpe), name))
  }

  private def addNameToGlobalScope(name: EXPR.Name, tpe: ESType): Unit = {
    scopes.lastOption.foreach(_.insert(Symbol(name.id.name, tpe), name))
  }

  /** Extracts types to be returned in the given number of statements (which form function body) */
  private def findReturnTypes(stmts: Seq[STMT]): Seq[ESType] = {

    def findReturnsIn(stmt: STMT): Seq[ESType] = stmt match {
      case ret: STMT.Return => Seq(ret.value.map(v => extractType(v.tpeOpt)).getOrElse(ESUnit))
      case STMT.If(_, body, orelse) => findReturnTypes(body) ++ findReturnTypes(orelse)
      case STMT.Match(_, branches) => findReturnTypes(branches)
      case STMT.Case(_, body, _) => findReturnTypes(body)
      case STMT.FunctionDef(_, _, body, _) => findReturnTypes(body)
      case _: STMT.Pass.type => Seq(ESUnit)
      case _ => Seq.empty
    }

    stmts.flatMap(findReturnsIn)
  }

  private def inferType(exp: EXPR): ESType = {

    val scope: ScopedSymbolTable = currentScopeOpt.getOrElse(throw MissedContextException(AstStringifier.toString(exp)))

    def inferTypeIn(e: EXPR): ESType = e.tpeOpt.getOrElse {
      exp match {
        case n: EXPR.Name => scope.lookup(n.id.name)
          .map(_.tpe).getOrElse(throw NameException(n.id.name, AstStringifier.toString(exp)))

        case attr: EXPR.Attribute =>
          inferType(attr.value) match {
            case p: ESProduct =>
              p.getAttrType(attr.attr.name)
                .getOrElse(throw NameException(attr.attr.name, AstStringifier.toString(exp)))
            case ESFunc(_, retT) => retT
            case _ => throw IllegalExprException(AstStringifier.toString(attr))
          }

        case fc: EXPR.Call =>
          fc.args.map(inferType)
          fc.func match {
            case EXPR.Name(n, _, _) =>
              scope.lookup(n.name).map { case Symbol(_, t) => t }
                .getOrElse(throw IllegalExprException(AstStringifier.toString(fc)))

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
                    case _ => throw IllegalExprException(AstStringifier.toString(exp))
                  }
                case _ => throw IllegalExprException(AstStringifier.toString(exp))
              }

            case EXPR.Attribute(value, n, _, _) =>
              inferType(value) match {
                case pt: ESProduct =>
                  pt.getAttrType(n.name) match {
                    case Some(funcT: ESFunc) => funcT
                    case _ => throw IllegalExprException(AstStringifier.toString(exp))
                  }
                case _ => throw IllegalExprException(AstStringifier.toString(exp))
              }

            case _ => throw IllegalExprException(AstStringifier.toString(exp))
          }

        case bop: EXPR.BinOp =>
          ESMath.ensureZeroDivision(bop.op, bop.right, exp)
          ESMath.BinaryOperationRuleset.find {
            case (op, (o1, o2), _) =>
              bop.op == op && o1 == inferType(bop.left) && o2 == inferType(bop.right)
          }.map(_._3).getOrElse(throw IllegalOperandException(AstStringifier.toString(exp)))

        case ifExp: EXPR.IfExp =>
          val bodyT: ESType = inferType(ifExp.body)
          val elseT: ESType = inferType(ifExp.orelse)
          if (bodyT != elseT) throw IllegalExprException(AstStringifier.toString(exp))
          bodyT

        case uop: EXPR.UnaryOp => inferType(uop.operand)

        case EXPR.ESList(elts, _, _) =>
          val listT: ESType = elts.headOption.map(inferType)
            .getOrElse(throw IllegalExprException(AstStringifier.toString(exp)))
          elts.tail.foreach(e => matchType(listT, inferType(e), exp))
          ensureNestedColl(elts)
          ESList(listT)

        case EXPR.ESDictNode(keys, vals, _) =>
          val keyT: ESType = keys.headOption.map(inferType).getOrElse(ESUnit)
          val valT: ESType = vals.headOption.map(inferType).getOrElse(ESUnit)
          keys.tail.foreach(k => matchType(keyT, inferType(k), exp))
          vals.tail.foreach(v => matchType(valT, inferType(v), exp))
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

        case _ => throw IllegalExprException(AstStringifier.toString(exp))
      }
    }

    val tpe = inferTypeIn(exp)
    if (exp.tpeOpt.isEmpty) exp.tpeOpt = Some(tpe)
    tpe
  }

  private def extractType(opt: Option[ESType]): ESType = opt.getOrElse(throw new Exception("Type extraction failed"))

  private def ensureNestedColl(exps: Seq[EXPR]): Unit = exps.foreach { exp =>
    val expT = exp.tpeOpt.get
    if (expT.isInstanceOf[ESList] || expT.isInstanceOf[ESDict])
      throw NestedCollectionException(exps.foldLeft("")((str, expr) => str.concat("\n" + AstStringifier.toString(expr))))
  }

  // TODO: Avoid passing unrelated argument `node`.
  private def matchType(t1: ESType, t2: ESType, node: AST_NODE): Unit =
    if (!(t1 == t2 || t2.isSubtypeOf(t1))) throw TypeMismatchException(t1.ident, t2.ident, AstStringifier.toString(node))

  private def assertDefined(n: String, node: AST_NODE): Unit =
    if (currentScopeOpt.flatMap(_.lookup(n)).isEmpty) throw NameException(n, AstStringifier.toString(node))
}

object StaticProcessor {

  def default: StaticProcessor = new StaticProcessor(TypeSystem.default)
}