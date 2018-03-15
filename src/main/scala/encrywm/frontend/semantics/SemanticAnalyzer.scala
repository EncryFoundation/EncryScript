package encrywm.frontend.semantics

import encrywm.frontend.parser.Ast._

class SemanticAnalyzer extends TreeNodeVisitor {

  private var currentScopeOpt: Option[ScopedSymbolTable] = None

  override def visit(node: AST_NODE): Unit = node match {
    case tr: TREE_ROOT => visitRoot(tr)
    case stmt: STMT => visitStmt(stmt)
    case expr: EXPR => visitExpr(expr)
    case _ => // Do nothing.
  }

  private def visitRoot(node: TREE_ROOT): Unit = node match {
    case c: TREE_ROOT.Contract =>
      currentScopeOpt = Some(InitialScope.global)
      c.body.foreach(visit)
    case _: TREE_ROOT.Expression => // Do we need this branch for scripts debugging?
    case _ => // Do nothing.
  }

  private def visitStmt(node: STMT): Unit = node match {

    case asg: STMT.Assign => asg.target match {
        case decl: EXPR.Decl => visitDecl(decl)
        case _: EXPR.Name => // Reassignment. This subject is under discussion now.
        case _ => // Do nothing.
      }
      visit(asg.value)

    case fd: STMT.FunctionDef =>
      assertDefined(fd.returnType.name)
      val returnTypeSymbol = BuiltInTypeSymbol(fd.returnType.name)
      currentScopeOpt.foreach(_.insert(FuncSymbol(fd.name.name, Some(returnTypeSymbol))))
      val fnScope = new ScopedSymbolTable(fd.name.name, currentScopeOpt.get.scopeLevel + 1, currentScopeOpt)
      currentScopeOpt = Some(fnScope)
      fd.args.args.foreach(visitDecl)
      fd.body.foreach(visit)

    case ret: STMT.Return => ret.value.foreach(visit)

    case _ => // Do nothing.
  }

  private def visitExpr(node: EXPR): Unit = node match {
    case n: EXPR.Name => assertDefined(n.id.name)
    case bo: EXPR.BoolOp => bo.values.foreach(visit)
    case bin: EXPR.BinOp =>
      // TODO: Ensure operands are compatible.
      visit(bin.left)
      visit(bin.right)
    case fc: EXPR.Call =>
    case _ => // Do nothing.
  }

  private def visitDecl(node: EXPR.Decl): Unit = node.target match {
    case n: EXPR.Name =>
      val typeSymbolOpt = node.typeOpt.map { t =>
        assertDefined(t.name)
        BuiltInTypeSymbol(t.name)
      }
      currentScopeOpt.foreach(_.insert(VariableSymbol(n.id.name, typeSymbolOpt)))
    case _ => // Do nothing.
  }

  private def assertDefined(n: String): Unit = if (currentScopeOpt.flatMap(_.lookup(n)).isEmpty) throw NameError(n)
}
