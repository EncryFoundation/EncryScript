package encrywm.frontend.semantics

import encrywm.builtins.ESMath
import encrywm.builtins.Types._
import encrywm.ast.Ast._
import encrywm.ast.AstNodeScanner
import encrywm.frontend.semantics.error._
import encrywm.frontend.semantics.scope._
import encrywm.utils.Stack
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Random

object StaticAnalyser extends AstNodeScanner {

  private lazy val scopes: Stack[ScopedSymbolTable] = new Stack

  private def currentScopeOpt: Option[ScopedSymbolTable] = scopes.currentOpt

  override def scan(node: AST_NODE): Unit = node match {
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

    case asg: STMT.Assign =>
      scan(asg.value)
      asg.target match {
        case EXPR.Declaration(name: EXPR.Name, typeOpt) =>
          val valueType = inferType(asg.value)
          val declTypeOpt = typeOpt.flatMap(t =>
            currentScopeOpt.map(_.lookup(t.name).map(s =>
              staticTypeById(s.name).getOrElse(TYPE_REF(s.name))
            ).getOrElse(throw NameError(t.name)))
          )
          declTypeOpt.foreach(tpe => assertEquals(tpe, valueType))
          addNameToScope(name, valueType)
        case _ => // ???
      }

    case fd: STMT.FunctionDef =>
      assertDefined(fd.returnType.name)
      val returnTypeSymbol = BuiltInTypeSymbol(fd.returnType.name)
      val paramSymbols = fd.args.args.map { arg =>
        arg.target match {
          case n: EXPR.Name =>
            val typeSymbolOpt = arg.typeOpt.map { t =>
              assertDefined(t.name)
              BuiltInTypeSymbol(t.name)
            }
            VariableSymbol(n.id.name, typeSymbolOpt)
          case _ => throw IllegalExprError
        }
      }
      currentScopeOpt.foreach(_.insert(FuncSymbol(fd.name.name, Some(returnTypeSymbol), paramSymbols)))
      val fnScope = ScopedSymbolTable(fd.name.name, currentScopeOpt.get)
      scopes.push(fnScope)
      paramSymbols.foreach(s => currentScopeOpt.foreach(_.insert(s)))
      fd.body.foreach(scan)

      val retType = findReturns(fd.body).map(_.value.map { exp =>
        scanExpr(exp)
        exp.tpeOpt.get
      }).foldLeft(Seq[TYPE]()) { case (acc, tOpt) =>
        val tpe = tOpt.getOrElse(UNIT)
        if (acc.nonEmpty) assertEquals(acc.head, tpe)
        acc :+ tpe
      }.headOption.getOrElse(UNIT)
      assertEquals(staticTypeById(fd.returnType.name).get, retType)

      scopes.popHead()

    case ret: STMT.Return => ret.value.foreach(scan)

    case expr: STMT.Expr =>
      scan(expr.value)
      inferType(expr.value)

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

    case STMT.UnlockIf(test) =>
      scanExpr(test)

    case _ => // Do nothing.
  }

  private def scanExpr(node: EXPR): Unit = {
    node match {
      case n: EXPR.Name =>
        assertDefined(n.id.name)

      case bo: EXPR.BoolOp =>
        bo.values.foreach(scanExpr)

      case bin: EXPR.BinOp =>
        Seq(bin.left, bin.right).foreach(scanExpr)

      case fc: EXPR.Call =>
        fc.func match {
          case n: EXPR.Name =>
            val fn = currentScopeOpt.flatMap(_.lookup(n.id.name))
              .getOrElse(throw NameError(n.id.name))
            if (fn.asInstanceOf[FuncSymbol].params.size != fc.args.size + fc.keywords.size)
              throw WrongNumberOfArgumentsError(fn.name)
            fc.args.foreach(scanExpr)
            fc.keywords.map(_.value).foreach(scanExpr)
          case _ => throw IllegalExprError
        }

      case attr: EXPR.Attribute =>
        if (!getAttributeBase(attr.value).attributes.map(_.name).contains(attr.attr.name))
          throw NameError(attr.attr.name)

      case cmp: EXPR.Compare =>
        cmp.comparators.foreach(scanExpr)
        scanExpr(cmp.left)

      case uop: EXPR.UnaryOp => scanExpr(uop.operand)

      case ifExp: EXPR.IfExp =>
        Seq(ifExp.test, ifExp.body, ifExp.orelse).foreach(scanExpr)

      case dct: EXPR.ESDict =>
        dct.keys.foreach(scan)
        dct.values.foreach(scan)

      case lst: EXPR.ESList => lst.elts.foreach(scanExpr)

      case sub: EXPR.Subscript =>
        scanExpr(sub.value)
        sub.slice match {
          case SLICE.Index(idx) =>
            scanExpr(idx)
            assertEquals(idx.tpeOpt.get, INT)

          // TODO: Complete for other SLICE_OPs.
        }

      case EXPR.Base58Str(s) =>
        if (Base58.decode(s).isFailure) throw Base58DecodeError

      case _ => // Do nothing.
    }
    inferType(node)
  }

  private def addNameToScope(name: EXPR.Name, tpe: TYPE): Unit = {
    val typeSymbol = tpe match {
      case LIST(valT) =>
        val valSymbol = BuiltInTypeSymbol(valT.identifier)
        BuiltInTypeSymbol(tpe.identifier, typeParams = Seq(valSymbol))

      case DICT(keyT, valT) =>
        val keySymbol = BuiltInTypeSymbol(keyT.identifier)
        val valSymbol = BuiltInTypeSymbol(valT.identifier)
        BuiltInTypeSymbol(tpe.identifier, typeParams = Seq(keySymbol, valSymbol))

      case _ => BuiltInTypeSymbol(tpe.identifier)
    }
    currentScopeOpt.foreach(_.insert(VariableSymbol(name.id.name, Some(typeSymbol))))
  }

  @tailrec
  def getAttributeBase(node: AST_NODE): BuiltInTypeSymbol = node match {
    case name: EXPR.Name =>
      val sym = currentScopeOpt.flatMap(_.lookup(name.id.name))
        .getOrElse(throw NameError(name.id.name))
      sym match {
        case bis: BuiltInTypeSymbol => bis
        case _ => throw NotAnObjectError(sym.name)
      }
    case at: EXPR.Attribute => getAttributeBase(at.value)
    case _ => throw IllegalExprError
  }

  private def findReturns(stmts: Seq[STMT]): Seq[STMT.Return] = {

    def findReturnsIn(stmt: STMT): Seq[STMT.Return] = stmt match {
      case ret: STMT.Return => Seq(ret)
      case ifStmt: STMT.If => findReturns(ifStmt.body) ++ findReturns(ifStmt.orelse)
      case fn: STMT.FunctionDef => findReturns(fn.body)
      case _ => Seq.empty
    }

    stmts.flatMap(findReturnsIn)
  }

  private def inferType(exp: EXPR): TYPE = {

    val scope = currentScopeOpt.getOrElse(throw MissedContextError)

    def inferTypeIn(e: EXPR): TYPE = e.tpeOpt.getOrElse {
      exp match {
        case n: EXPR.Name => scope.lookup(n.id.name)
          .map { symbol =>
            val symbolT = symbol.tpeOpt.get
            staticTypeById(symbolT.name).getOrElse {
              if (symbolT.name == "list") {
                val valT = staticTypeById(symbolT.typeParams.head.name).getOrElse(TYPE_REF(symbolT.name))
                LIST(valT)
              } else if (symbolT.name == "dict") {
                val keyT = staticTypeById(symbolT.typeParams.head.name).getOrElse(TYPE_REF(symbolT.name))
                val valT = staticTypeById(symbolT.typeParams.last.name).getOrElse(TYPE_REF(symbolT.name))
                DICT(keyT, valT)
              } else {
                TYPE_REF(symbolT.name)
              }
            }
          }.getOrElse(throw NameError(n.id.name))

        case a: EXPR.Attribute =>
          getAttributeBase(a).attributes.find(s => s.name == a.attr.name).get.tpeOpt.map(s =>
            staticTypeById(s.name).getOrElse(TYPE_REF(s.name))).getOrElse(throw TypeError)

        case fc: EXPR.Call =>
          fc.func match {
            case n: EXPR.Name =>
              scope.lookup(n.id.name).map { case sym: FuncSymbol =>
                val args = sym.params.map(p => p.tpeOpt.get)
                fc.args.map(inferType).zip(args).foreach { case (t1, t2s) =>
                  if (t1 != t2s) throw TypeMismatchError(t1.identifier, t2s.name)
                }
                sym.tpeOpt.flatMap(r => staticTypeById(r.name))
                  .getOrElse(throw new SemanticError("Illegal return type."))
              }.getOrElse(throw IllegalExprError)

            case _ => throw IllegalExprError
          }

        case bop: EXPR.BinOp =>
          ESMath.ensureZeroDivision(bop.op, bop.right)
          ESMath.BinaryOperationResults.find {
            case (op, (o1, o2), _) =>
              bop.op == op && o1 == inferType(bop.left) && o2 == inferType(bop.right)
          }.map(_._3).getOrElse(throw IllegalOperandError)

        case ifExp: EXPR.IfExp =>
          val bodyType = inferType(ifExp.body)
          val elseType = inferType(ifExp.orelse)
          if (bodyType != elseType) throw IllegalExprError
          bodyType

        case uop: EXPR.UnaryOp => inferType(uop.operand)

        case EXPR.ESList(elts, _, _) =>
          val listT = elts.headOption.map(inferType).getOrElse(UNIT)  // TODO: Allow creating empty colls?
          elts.tail.foreach(e => assertEquals(listT, inferType(e)))
          ensureNestedColl(elts)
          LIST(listT)

        case EXPR.ESDict(keys, vals, _) =>
          val keyT = keys.headOption.map(inferType).getOrElse(UNIT)
          val valT = vals.headOption.map(inferType).getOrElse(UNIT)
          keys.tail.foreach(k => assertEquals(keyT, inferType(k)))
          vals.tail.foreach(v => assertEquals(valT, inferType(v)))
          ensureNestedColl(vals)  // TODO: Ensure nested coll for keys?
          DICT(keyT, valT)

        case EXPR.Subscript(value, SLICE.Index(_), _, _) =>
          inferType(value) match {
            case list: LIST => list.valT
            case dict: DICT => dict.valT
          }

        case _ => throw IllegalExprError
      }
    }

    val tpe = inferTypeIn(exp)
    if (exp.tpeOpt.isEmpty) exp.tpeOpt = Some(tpe)
    tpe
  }

  private def ensureNestedColl(exps: Seq[EXPR]): Unit = exps.foreach { exp =>
    val expT = exp.tpeOpt.get
    if (expT.isInstanceOf[LIST] || expT.isInstanceOf[DICT]) throw NestedCollectionError
  }

  private def assertEquals(t1: TYPE, t2: TYPE): Unit =
    if (t1 != t2) throw TypeMismatchError(t1.identifier, t2.identifier)

  private def assertDefined(n: String): Unit = if (currentScopeOpt.flatMap(_.lookup(n)).isEmpty) throw NameError(n)
}
