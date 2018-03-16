package encrywm.frontend.semantics

import encrywm.builtins.{Builtins, ESMath}
import encrywm.frontend.parser.Ast.{EXPR, TYPE, _}
import encrywm.frontend.semantics.error.{IllegalExprError, IllegalOperandError, NameError, TypeMismatchError}
import encrywm.frontend.semantics.scope.ScopedSymbolTable

class TypeScanner(val tree: TREE_ROOT, val scope: ScopedSymbolTable) extends TreeNodeScanner {

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
          decl.typeOpt.foreach(typeId => assertEquals(typeFromId(typeId), valueType))
        case _ => // Do nothing.
      }

    case fd: STMT.FunctionDef =>
      val retType = findReturns(fd.body).map(_.value.map(getType)).foldLeft(Seq[TYPE]()) { case (acc, tOpt) =>
        val tpe = tOpt.getOrElse(TYPE.UNIT)
        if (acc.nonEmpty) assertEquals(acc.head, tpe)
        acc :+ tpe
      }.headOption.getOrElse(TYPE.UNIT)
      assertEquals(typeFromId(fd.returnType), retType)
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

    def getAndSetType(e: EXPR): TYPE = e match {
      case expr: EXPR.TYPED_EXPR =>
        val tpe = getType(expr)
        if (expr.tpeOpt.isEmpty) expr.tpeOpt = Some(tpe)
        tpe
      case _ => throw IllegalExprError
    }

    exp match {
      case expr: EXPR.TYPED_EXPR => expr.tpeOpt.getOrElse {
        expr match {
          case n: EXPR.Name => scope.lookup(n.id.name)
            .map(r => Builtins.StaticBuiltInTypes.find(t => t.symbol.name == r.tpeOpt.get.name).get.astType) // TODO: .get
            .getOrElse(throw NameError(n.id.name))

          case fc: EXPR.Call =>
            // TODO: Arguments checks.
            getAndSetType(fc.func)

          case bop: EXPR.BinOp =>
            (bop.left, bop.right) match {
              case (lt: EXPR.TYPED_EXPR, rt: EXPR.TYPED_EXPR) =>
                ESMath.ensureZeroDivision(bop.op, rt)
                ESMath.BinaryOperationResults.find {
                  case (op, (o1, o2), _) => bop.op == op && o1 == getAndSetType(lt) && o2 == getAndSetType(rt)
                }.map(_._3).getOrElse(throw IllegalOperandError)
              case _ => throw IllegalExprError
            }

          case ifExp: EXPR.IfExp =>
            val bodyType = getAndSetType(ifExp.body)
            val elseType = getAndSetType(ifExp.orelse)
            if (bodyType != elseType) throw IllegalExprError
            bodyType

          case uop: EXPR.UnaryOp => getAndSetType(uop.operand)

          case _ => throw IllegalExprError
        }
      }
      case _ => throw IllegalExprError
    }
  }

  private def typeFromId(id: Identifier): TYPE =
    Builtins.StaticBuiltInTypes.find(t => t.symbol.name == id.name).get.astType

  private def assertEquals(t1: TYPE, t2: TYPE): Unit =
    if (t1 != t2) throw TypeMismatchError(t1.name, t2.name)
}
