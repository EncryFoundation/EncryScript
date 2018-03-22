package encrywm.frontend.semantics

import encrywm.builtins.ESMath
import encrywm.builtins.Types._
import encrywm.ast.Ast._
import encrywm.ast.TreeNodeScanner
import encrywm.frontend.semantics.error._
import encrywm.frontend.semantics.scope._
import encrywm.utils.Stack

import scala.annotation.tailrec
import scala.util.Random

object StaticAnalyser extends TreeNodeScanner {

  private lazy val scopes: Stack[ScopedSymbolTable] = new Stack

  private def currentScopeOpt: Option[ScopedSymbolTable] = scopes.currentOpt

  override def scan(node: AST_NODE): Unit = node match {
    case tr: TREE_ROOT => scanRoot(tr)
    case stmt: STMT => scanStmt(stmt)
    case expr: EXPR => scanExpr(expr)
    case _ => // Do nothing.
  }

  private def scanRoot(node: TREE_ROOT): Unit = node match {
    case c: TREE_ROOT.Contract =>
      scopes.push(InitialScope.global)
      c.body.foreach(scan)
    case _ => // Do nothing.
  }

  private def scanStmt(node: STMT): Unit = node match {

    case asg: STMT.Assign =>
      scan(asg.value)
      asg.target match {
        case decl: EXPR.Decl =>
          val valueType = inferType(asg.value)
          val declTypeOpt = decl.typeOpt.flatMap(t =>
            currentScopeOpt.map(_.lookup(t.name).map(s =>
              staticTypeById(s.name).getOrElse(TYPE_REF(s.name))
            ).getOrElse(throw NameError(t.name)))
          )
          declTypeOpt.foreach(tpe => assertEquals(tpe, valueType))
          addNameToScope(decl.target, valueType)
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
        inferType(exp)
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

    case _ => // Do nothing.
  }

  private def scanExpr(node: EXPR): Unit = {
    node match {
      case n: EXPR.Name =>
        assertDefined(n.id.name)

      case bo: EXPR.BoolOp =>
        bo.values.foreach(scan)

      case bin: EXPR.BinOp =>
        Seq(bin.left, bin.right).foreach(scan)

      case fc: EXPR.Call =>
        fc.func match {
          case n: EXPR.Name =>
            val fn = currentScopeOpt.flatMap(_.lookup(n.id.name))
              .getOrElse(throw NameError(n.id.name))
            if (fn.asInstanceOf[FuncSymbol].params.size != fc.args.size + fc.keywords.size)
              throw WrongNumberOfArgumentsError(fn.name)
            fc.args.foreach(scan)
            fc.keywords.map(_.value).foreach(scan)
          case _ => throw IllegalExprError
        }

      case attr: EXPR.Attribute =>
        if (!getAttributeBase(attr.value).attributes.map(_.name).contains(attr.attr.name))
          throw NameError(attr.attr.name)

      case cmp: EXPR.Compare =>
        cmp.comparators.foreach(scan)
        scan(cmp.left)

      case uop: EXPR.UnaryOp => scan(uop.operand)

      case ifExp: EXPR.IfExp =>
        Seq(ifExp.test, ifExp.body, ifExp.orelse).foreach(scan)

      case dct: EXPR.Dict =>
        dct.keys.foreach(scan)
        dct.values.foreach(scan)

      case lst: EXPR.EList => lst.elts.foreach(scan)

      case _ => // Do nothing.
    }
    inferType(node)
  }

  private def addNameToScope(node: EXPR, tpe: TYPE): Unit = node match {
    case n: EXPR.Name =>
      val typeSymb = BuiltInTypeSymbol(tpe.identifier)
      currentScopeOpt.foreach(_.insert(VariableSymbol(n.id.name, Some(typeSymb))))
    case _ => throw IllegalExprError
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
          .map(r => staticTypeById(r.tpeOpt.get.name).getOrElse(TYPE_REF(r.name))) // TODO: .get
          .getOrElse(throw NameError(n.id.name))

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

        case _ => throw IllegalExprError
      }
    }

    val tpe = inferTypeIn(exp)
    if (exp.tpeOpt.isEmpty) exp.tpeOpt = Some(tpe)
    tpe
  }

  private def assertEquals(t1: TYPE, t2: TYPE): Unit =
    if (t1 != t2) throw TypeMismatchError(t1.identifier, t2.identifier)

  private def assertDefined(n: String): Unit = if (currentScopeOpt.flatMap(_.lookup(n)).isEmpty) throw NameError(n)
}
