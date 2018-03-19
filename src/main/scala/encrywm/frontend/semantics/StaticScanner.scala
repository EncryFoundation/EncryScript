package encrywm.frontend.semantics

import encrywm.frontend.ast.Ast._
import encrywm.frontend.semantics.error._
import encrywm.frontend.semantics.scope._

import scala.annotation.tailrec

object StaticScanner extends TreeNodeScanner {

  private var currentScopeOpt: Option[ScopedSymbolTable] = None

  def scanTreeAndGetSymtable(node: TREE_ROOT): ScopedSymbolTable = {
    scanRoot(node)
    currentScopeOpt.getOrElse(throw MissedContextError)
  }

  override def scan(node: AST_NODE): Unit = node match {
    case tr: TREE_ROOT => scanRoot(tr)
    case stmt: STMT => scanStmt(stmt)
    case expr: EXPR => scanExpr(expr)
    case _ => // Do nothing.
  }

  private def scanRoot(node: TREE_ROOT): Unit = node match {
    case c: TREE_ROOT.Contract =>
      currentScopeOpt = Some(InitialScope.global)
      c.body.foreach(scan)
    case _ => // Do nothing.
  }

  private def scanStmt(node: STMT): Unit = node match {

    case asg: STMT.Assign => asg.target match {
        case decl: EXPR.Decl => scanDecl(decl)
        case _ => // Do nothing.
      }
      scan(asg.value)

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
      val fnScope = new ScopedSymbolTable(fd.name.name, currentScopeOpt.get.scopeLevel + 1, currentScopeOpt)
      currentScopeOpt = Some(fnScope)
      paramSymbols.foreach(s => currentScopeOpt.foreach(_.insert(s)))
      fd.body.foreach(scan)

    case ret: STMT.Return => ret.value.foreach(scan)

    case expr: STMT.Expr => scan(expr.value)

    case ifStmt: STMT.If =>
      scan(ifStmt.test)
      ifStmt.body.foreach(scan)
      ifStmt.orelse.foreach(scan)

    case _ => // Do nothing.
  }

  private def scanExpr(node: EXPR): Unit = node match {

    case n: EXPR.Name => assertDefined(n.id.name)

    case bo: EXPR.BoolOp => bo.values.foreach(scan)

    case bin: EXPR.BinOp =>
      scan(bin.left)
      scan(bin.right)

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
      @tailrec
      def getBase(node: AST_NODE): BuiltInTypeSymbol = node match {
        case name: EXPR.Name =>
          val sym = currentScopeOpt.flatMap(_.lookup(name.id.name))
            .getOrElse(throw NameError(name.id.name))
          sym match {
            case bis: BuiltInTypeSymbol => bis
            case _ => throw NotAnObjectError(sym.name)
          }
        case at: EXPR.Attribute => getBase(at.value)
        case _ => throw IllegalExprError
      }
      if (!getBase(attr.value).attributes.map(_.name).contains(attr.attr.name))
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

  private def scanDecl(node: EXPR.Decl): Unit = node.target match {
    case n: EXPR.Name =>
      val typeSymbolOpt = node.typeOpt.map { t =>
        assertDefined(t.name)
        BuiltInTypeSymbol(t.name)
      }
      currentScopeOpt.foreach(_.insert(VariableSymbol(n.id.name, typeSymbolOpt)))
    case _ => throw IllegalExprError
  }

  private def assertDefined(n: String): Unit = if (currentScopeOpt.flatMap(_.lookup(n)).isEmpty) throw NameError(n)
}
