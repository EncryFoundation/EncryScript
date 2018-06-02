package encrywm.lang.frontend.semantics

import encrywm.ast.Ast._
import encrywm.lib.predef.PredefFunctions

object ComplexityAnalyzer {

  type ScriptComplexityScore = Int

  def complexityOf(node: AST_NODE): ScriptComplexityScore = node match {
    case root: TREE_ROOT => scanRoot(root)
    case stmt: STMT => scanStmt(stmt)
    case expr: EXPR => scanExpr(expr)
    case _ => 0
  }

  private def scanRoot(root: TREE_ROOT): ScriptComplexityScore = root match {
    case c: TREE_ROOT.Contract => c.body.map(complexityOf).sum
    case _ => 0
  }

  private def scanStmt(stmt: STMT): ScriptComplexityScore = stmt match {
    case STMT.FunctionDef(_, _, body, _) => 1 + body.map(scanStmt).sum
    case STMT.Return(value) => value.map(scanExpr).getOrElse(0)
    case STMT.Let(_, value) => scanExpr(value)
    case STMT.If(test, body, orelse) => 1 + scanExpr(test) + Math.max(body.map(scanStmt).sum, orelse.map(scanStmt).sum)
    case STMT.Assert(test, msg) => scanExpr(test) + msg.map(scanExpr).getOrElse(0)
    case STMT.Expr(value) => scanExpr(value)
    case STMT.UnlockIf(expr) => scanExpr(expr)
    case STMT.Halt => 1
    case STMT.Pass => 1
    case _ => 0
  }

  // FIXME: User-defined function call complexity estimated incorrectly.
  private def scanExpr(expr: EXPR): ScriptComplexityScore = expr match {
    case EXPR.BoolOp(_, values) => values.length
    case EXPR.BinOp(left, _, right, _) => scanExpr(left) + scanExpr(right) + 1
    case EXPR.UnaryOp(_, operand, _) => scanExpr(operand)
    case EXPR.Lambda(_, body, _) => scanExpr(body)
    case EXPR.IfExp(test, body, orelse, _) => scanExpr(test) + Math.max(scanExpr(body), scanExpr(orelse))
    case EXPR.Compare(left, ops, comparators) => scanExpr(left) + ops.length + comparators.map(scanExpr).sum
    case EXPR.Call(EXPR.Name(Identifier(n), _), args, _, _) => args.map(scanExpr).sum + {
      if (PredefFunctions.hashFunctions.map(_.name).contains(n)) 10
      else if (PredefFunctions.middleFunctions.map(_.name).contains(n)) 15
      else if (PredefFunctions.heavyFunctions.map(_.name).contains(n)) 20
      else 2
    }
    case EXPR.IntConst(_) => 1
    case EXPR.LongConst(_) => 1
    case EXPR.True => 1
    case EXPR.False => 1
    case EXPR.Str(_) => 1
    case EXPR.Base58Str(_) => 1
    case EXPR.Attribute(value, _, _) => scanExpr(value)
    case EXPR.Subscript(value, _, _) => scanExpr(value)
    case EXPR.ESDictNode(keys, values, _) => keys.map(scanExpr).sum + values.map(scanExpr).sum
    case EXPR.ESSet(elts, _) => elts.map(scanExpr).sum
    case EXPR.ESList(elts, _) => elts.map(scanExpr).sum
    case EXPR.ESTuple(elts, _) => elts.map(scanExpr).sum
    case EXPR.Declaration(target, _) => scanExpr(target)
    case EXPR.TypeMatching(_, tipe) => 2 + tipe.typeParams.length
    case EXPR.GenericCond => 1
    case _ => 0
  }
}
