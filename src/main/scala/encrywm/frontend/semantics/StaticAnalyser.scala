package encrywm.frontend.semantics

import encrywm.ast.Ast._
import encrywm.ast.{AstNodeScanner, AstString}
import encrywm.frontend.semantics.error._
import encrywm.frontend.semantics.scope._
import encrywm.lib.Types._
import encrywm.lib.{ESMath, Types}
import encrywm.utils.Stack
import scorex.crypto.encode.Base58

import scala.util.Random

class StaticAnalyser extends AstNodeScanner {

  private lazy val scopes: Stack[ScopedSymbolTable] = new Stack

  private def currentScopeOpt: Option[ScopedSymbolTable] = scopes.currentOpt

  override def scan(node: AST_NODE): Unit = {
    node match {
      case root: TREE_ROOT =>
        scanRoot(root)
      case stmt: STMT =>
        scanStmt(stmt)
      case expr: EXPR =>
        scanExpr(expr)
      case _ => // Do nothing.
    }
  }

  private def scanRoot(node: TREE_ROOT): Unit = node match {
    case c: TREE_ROOT.Contract =>
      scopes.push(ScopedSymbolTable.initialized)
      c.body.foreach(scan)
    case _ => // Do nothing.
  }

  private def scanStmt(node: STMT): Unit = node match {

    case asg: STMT.Let =>
      //println(s"current string is: $currentString")
      scan(asg.value)
      asg.target match {
        case EXPR.Declaration(name: EXPR.Name, typeOpt) =>
          val valueType = inferType(asg.value)
          val typeDeclOpt = typeOpt.map { t =>
            val mainT = typeByIdent(t.ident.name).getOrElse(throw NameError(t.ident.name, AstString.toString(asg)))
            val typeParams = t.typeParams.map(id => typeByIdent(id.name).getOrElse(throw NameError(id.name, AstString.toString(asg))))
            mainT -> typeParams
          }
          typeDeclOpt.foreach {
            case (ESOption(_), typeParams) => matchType(ESOption(typeParams.head), valueType, node)
            case (ESList(_), typeParams) => matchType(ESList(typeParams.head), valueType, node)
            case (ESDict(_, _), typeParams) => matchType(ESDict(typeParams.head, typeParams.last), valueType, node)
            case (otherT, _) => matchType(otherT, valueType, node)
          }
          if (asg.global) addNameToGlobalScope(name, valueType)
          else addNameToScope(name, valueType)
        case _ => throw IllegalExprError(AstString.toString(asg))
      }

    case fd: STMT.FunctionDef =>
      val declaredRetType = typeByIdent(fd.returnType.name)
        .getOrElse(throw NameError(fd.returnType.name, AstString.toString(fd)))
      val params = fd.args.args.map { arg =>
        val argT = typeByIdent(arg._2.ident.name).getOrElse(throw UnresolvedSymbolError(arg._2.ident.name, AstString.toString(fd)))
        arg._1.name -> argT
      }
      currentScopeOpt.foreach(_.insert(Symbol(fd.name.name, ESFunc(params, declaredRetType)), node))
      val fnScope = ScopedSymbolTable(fd.name.name, currentScopeOpt.get)
      scopes.push(fnScope)
      params.foreach(p => currentScopeOpt.foreach(_.insert(Symbol(p._1, p._2), node)))
      fd.body.foreach(scan)

      val retType = findReturns(fd.body).map(_.value.map { exp =>
        exp.tpeOpt.get
      }).foldLeft(Seq[ESType]()) { case (acc, tOpt) =>
        val tpe = tOpt.getOrElse(ESUnit)
        if (acc.nonEmpty) matchType(acc.head, tpe, node)
        acc :+ tpe
      }.headOption.getOrElse(ESUnit)
      matchType(declaredRetType, retType, node)

      scopes.popHead()

    case ret: STMT.Return =>
      ret.value.foreach(expr => scan(expr))

    case expr: STMT.Expr =>
      scan(expr.value)

    case ifStmt: STMT.If =>
      scan(ifStmt.test)
      val bodyScope = ScopedSymbolTable(s"if_body_${Random.nextInt()}", currentScopeOpt.get)
      scopes.push(bodyScope)
      ifStmt.body.foreach(scan)
      scopes.popHead()
      val elseScope = ScopedSymbolTable(s"if_else_${Random.nextInt()}", currentScopeOpt.get)
      scopes.push(elseScope)
      ifStmt.orelse.foreach(scan)
      scopes.popHead()

    case STMT.Match(target, branches) =>
      scanExpr(target)
      if (!branches.forall(_.isInstanceOf[STMT.Case]))
        throw UnexpectedStatementError("Case clause is expected", branches.foldLeft("")((str, branch) => str.concat(AstString.toString(branch))))
      else if (!branches.last.asInstanceOf[STMT.Case].isDefault)
        throw DefaultBranchUndefinedError(AstString.toString(branches.last))
      branches.foreach(scan)

    case STMT.Case(cond, body, _) =>
      scan(cond)
      val bodyScope = ScopedSymbolTable(s"case_branch_${Random.nextInt()}", currentScopeOpt.get)
      scopes.push(bodyScope)
      cond match {
        case EXPR.BranchParamDeclaration(local, tpe) =>
          val localT = typeByIdent(tpe.ident.name).getOrElse(throw TypeError(AstString.toString(cond)))
          currentScopeOpt.foreach(_.insert(Symbol(local.name, localT), node))
        case _ => // Do nothing.
      }
      body.foreach(scan)
      scopes.popHead()

    case STMT.UnlockIf(test) =>
      scan(test)

    case _ => // Do nothing.
  }

  private def scanExpr(node: EXPR): Unit = {
    def scanMany(exprs: Seq[EXPR]): Unit =
      exprs.foreach(expr => scan(expr))
    node match {
      case n: EXPR.Name =>
        assertDefined(n.id.name, node)

      case bo: EXPR.BoolOp =>
        bo.values.foreach(expr => scan(expr))

      case bin: EXPR.BinOp =>
        Seq(bin.left, bin.right).foreach(expr => scan(expr))

      case EXPR.Lambda(args, body, _) =>
        val paramSymbols = args.args.map { arg =>
          val argT = typeByIdent(arg._2.ident.name).getOrElse(throw UnresolvedSymbolError(arg._2.ident.name, AstString.toString(node)))
          Symbol(arg._1.name, argT)
        }
        val bodyScope = ScopedSymbolTable(s"lamb_body_${Random.nextInt()}", currentScopeOpt.get)
        scopes.push(bodyScope)
        paramSymbols.foreach(s => currentScopeOpt.foreach(_.insert(s, node)))
        scan(body)
        scopes.popHead()

      case EXPR.Call(EXPR.Name(id, _, _), args, keywords, _) =>
        currentScopeOpt.flatMap(_.lookup(id.name)).map { case Symbol(_, ESFunc(params, _)) =>
          if (params.size != args.size + keywords.size) throw WrongNumberOfArgumentsError(id.name, AstString.toString(node))
          val argTypes = params.map(_._2)
          args.map(arg => inferType(arg)).zip(argTypes).foreach { case (t1, t2) =>
            matchType(t1, t2, node)
          }
          id.name
        }.getOrElse(throw NameError(id.name, AstString.toString(node)))
        args.foreach(expr => scan(expr))
        keywords.map(_.value).foreach(expr => scan(expr))

      case EXPR.Call(func: EXPR.Attribute, args, keywords, _) =>
        scanMany(Seq(func, func.value) ++ args ++ keywords.map(_.value))
        func.value.tpeOpt.get match {
          case coll: ESCollection if args.size == 1 =>
            coll.getAttrType(func.attr.name) match {
              case Some(f: ESFunc) =>
                args.map(arg => inferType(arg)).zip(f.args.map(_._2)).foreach { case (t1, t2) =>
                  matchType(t1, t2, node)
                }
            }
          case _ =>
            currentScopeOpt.flatMap(_.lookup(func.attr.name)).map { case Symbol(_, ESFunc(params, _)) =>
              if (params.size != args.size + keywords.size) throw WrongNumberOfArgumentsError(func.attr.name, AstString.toString(node))
              val argTypes = params.map(_._2)
              args.map(arg => inferType(arg)).zip(argTypes).foreach { case (t1, t2) =>
                matchType(t1, t2, node)
              }
              func.attr.name
            }.getOrElse(throw NameError(func.attr.name, AstString.toString(node)))
        }

      case cmp: EXPR.Compare =>
        cmp.comparators.foreach(expr => scan(expr))
        scan(cmp.left)

      case uop: EXPR.UnaryOp => scan(uop.operand)

      case ifExp: EXPR.IfExp =>
        Seq(ifExp.test, ifExp.body, ifExp.orelse).foreach(expr => scan(expr))

      case dct: EXPR.ESDictNode =>
        dct.keys.foreach(expr => scan(expr))
        if (!dct.keys.forall(k => k.tpeOpt.get.isPrimitive)) throw IllegalExprError(AstString.toString(node))
        dct.values.foreach(expr => scan(expr))

      case lst: EXPR.ESList => lst.elts.foreach(expr => scan(expr))

      case sub: EXPR.Subscript =>
        scan(sub.value)
        sub.slice match {
          case SLICE.Index(idx) =>
            scan(idx)
            val idxT = idx.tpeOpt.get
            sub.value.tpeOpt match {
              case Some(ESList(_)) => matchType(idxT, ESInt, node)
              case Some(ESDict(keyT, _)) => matchType(idxT, keyT, node)
              case _ => throw IllegalExprError(AstString.toString(sub))
            }
          // TODO: Complete for other SLICE_OPs.
        }

      case EXPR.BranchParamDeclaration(_, tpe) =>
        typeByIdent(tpe.ident.name).getOrElse(throw UnresolvedSymbolError(tpe.ident.name, AstString.toString(node)))

      case EXPR.Base58Str(s) =>
        if (Base58.decode(s).isFailure) throw Base58DecodeError(AstString.toString(node))

      case _ => // Do nothing.
    }
    inferType(node)
  }

  private def addNameToScope(name: EXPR.Name, tpe: ESType): Unit = {
    currentScopeOpt.foreach(_.insert(Symbol(name.id.name, tpe), name))
  }

  private def addNameToGlobalScope(name: EXPR.Name, tpe: ESType): Unit = {
    scopes.lastOpt.foreach(_.insert(Symbol(name.id.name, tpe), name))
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

    val scope = currentScopeOpt.getOrElse(throw MissedContextError(AstString.toString(exp)))

    def inferTypeIn(e: EXPR): ESType = e.tpeOpt.getOrElse {
      exp match {
        case n: EXPR.Name => scope.lookup(n.id.name)
          .map(_.tpe).getOrElse(throw NameError(n.id.name, AstString.toString(exp)))

        case attr: EXPR.Attribute =>
          inferType(attr.value) match {
            case p: ESProduct =>
              p.getAttrType(attr.attr.name).getOrElse(throw NameError(attr.attr.name, AstString.toString(exp)))
          }

        case fc: EXPR.Call =>
          fc.func match {
            case EXPR.Name(n, _, _) =>
              scope.lookup(n.name).map { case Symbol(_, t) => t }
                .getOrElse(throw IllegalExprError(AstString.toString(exp)))

            // Special handler for `.map()`
            case EXPR.Attribute(value, n, _, _)
              if n.name == "map" && fc.args.size == 1 =>
              inferType(value) match {
                case coll: ESCollection =>
                  coll.getAttrType(n.name) match {
                    case Some(_: ESFunc) =>
                      inferType(fc.args.head) match {
                        case ESFunc(args, retT) => ESList(retT)
                      }
                    case _ => throw IllegalExprError(AstString.toString(exp))
                  }
                case _ => throw IllegalExprError(AstString.toString(exp))
              }

            case EXPR.Attribute(value, n, _, _) =>
              inferType(value) match {
                case pt: ESProduct =>
                  pt.getAttrType(n.name) match {
                    case Some(funcT: ESFunc) => funcT
                    case _ => throw IllegalExprError(AstString.toString(exp))
                  }
                case _ => throw IllegalExprError(AstString.toString(exp))
              }

            case _ => throw IllegalExprError(AstString.toString(exp))
          }

        case bop: EXPR.BinOp =>
          ESMath.ensureZeroDivision(bop.op, bop.right, exp)
          ESMath.BinaryOperationResults.find {
            case (op, (o1, o2), _) =>
              bop.op == op && o1 == inferType(bop.left) && o2 == inferType(bop.right)
          }.map(_._3).getOrElse(throw IllegalOperandError(AstString.toString(exp)))

        case ifExp: EXPR.IfExp =>
          val bodyType = inferType(ifExp.body)
          val elseType = inferType(ifExp.orelse)
          if (bodyType != elseType) throw IllegalExprError(AstString.toString(exp))
          bodyType

        case uop: EXPR.UnaryOp => inferType(uop.operand)

        case EXPR.ESList(elts, _, _) =>
          val listT = elts.headOption.map(elt => inferType(elt))
            .getOrElse(throw IllegalExprError(AstString.toString(exp)))
          elts.tail.foreach(e => matchType(listT, inferType(e), exp))
          ensureNestedColl(elts)
          ESList(listT)

        case EXPR.ESDictNode(keys, vals, _) =>
          val keyT = keys.headOption.map(elt => inferType(elt)).getOrElse(ESUnit)
          val valT = vals.headOption.map(elt => inferType(elt)).getOrElse(ESUnit)
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
            argId.name -> Types.typeByIdent(typeId.ident).get }, inferType(body))

        case _ => throw IllegalExprError(AstString.toString(exp))
      }
    }

    val tpe = inferTypeIn(exp)
    if (exp.tpeOpt.isEmpty) exp.tpeOpt = Some(tpe)
    tpe
  }

  private def ensureNestedColl(exps: Seq[EXPR]): Unit = exps.foreach { exp =>
    val expT = exp.tpeOpt.get
    if (expT.isInstanceOf[ESList] || expT.isInstanceOf[ESDict]) throw NestedCollectionError(exps.foldLeft("")((str, expr) => str.concat("\n" + AstString.toString(expr))))
  }

  private def matchType(t1: ESType, t2: ESType, node: AST_NODE): Unit =
    if (!(t1 == t2 || t2.isSubtypeOf(t1))) throw TypeMismatchError(t1.ident, t2.ident, AstString.toString(node))


  private def assertDefined(n: String, node: AST_NODE): Unit = if (currentScopeOpt.flatMap(_.lookup(n)).isEmpty) throw NameError(n, AstString.toString(node))
}
