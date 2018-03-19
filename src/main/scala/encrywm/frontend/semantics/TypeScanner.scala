package encrywm.frontend.semantics

import encrywm.builtins.ESMath
import encrywm.frontend.ast.Ast.{EXPR, _}
import encrywm.frontend.semantics.error._
import encrywm.frontend.semantics.scope.{FuncSymbol, ScopedSymbolTable}

class TypeScanner(val tree: TREE_ROOT, val scope: ScopedSymbolTable) extends TreeNodeScanner {

  import encrywm.builtins.Types._

  def processedTree: TREE_ROOT = {
    scanRoot(tree)
    tree
  }

  override def scan(node: AST_NODE): Unit = node match {
    case tr: TREE_ROOT => scanRoot(tr)
    case stmt: STMT => scanStmt(stmt)
    case _ => // Do nothing.
  }

  private def scanRoot(node: TREE_ROOT): Unit = node match {
    case c: TREE_ROOT.Contract => c.body.foreach(scan)
    case _ => // Do nothing.
  }

  private def scanStmt(node: STMT): Unit = node match {

    case asg: STMT.Assign => asg.target match {
        case decl: EXPR.Decl =>
          val valueType = getType(asg.value)
          val declTypeOpt = decl.typeOpt.map(t => scope.lookup(t.name).map(s => TYPE_REF(s.name))
            .getOrElse(throw NameError(t.name)))
          declTypeOpt.foreach(tpe => assertEquals(tpe, valueType))
        case _ => // Do nothing.
      }

    case fd: STMT.FunctionDef =>
      val retType = findReturns(fd.body).map(_.value.map(getType)).foldLeft(Seq[TYPE]()) { case (acc, tOpt) =>
        val tpe = tOpt.getOrElse(UNIT)
        if (acc.nonEmpty) assertEquals(acc.head, tpe)
        acc :+ tpe
      }.headOption.getOrElse(UNIT)
      assertEquals(staticTypeById(fd.returnType.name).get, retType)

    case expr: STMT.Expr => getType(expr.value)

    case _ => // Do nothing.
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

  private def getType(exp: EXPR): TYPE = {

    def inferType(e: EXPR): TYPE = {
      val tpe = getType(e)
      if (e.tpeOpt.isEmpty) e.tpeOpt = Some(tpe)
      tpe
    }

    exp.tpeOpt.getOrElse {
      exp match {
        case n: EXPR.Name => scope.lookup(n.id.name)
          .map(r => staticTypeById(r.tpeOpt.get.name).getOrElse(TYPE_REF(r.name))) // TODO: .get
          .getOrElse(throw NameError(n.id.name))

        case fc: EXPR.Call =>
          fc.func match {
            case n: EXPR.Name =>
              scope.lookup(n.id.name).map { case sym: FuncSymbol =>
                val args = sym.params.map(p => p.tpeOpt.get)
                fc.args.map(inferType).zip(args).foreach { case (t1, t2s) =>
                  if (t1.identifier != t2s.name) throw TypeMismatchError(t1.identifier, t2s.name) // TODO: Compare types properly.
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
  }

  private def assertEquals(t1: TYPE, t2: TYPE): Unit =
    if (t1 != t2) throw TypeMismatchError(t1.identifier, t2.identifier)
}
