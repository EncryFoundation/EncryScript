package encrywm.frontend.semantics

import encrywm.ast.Ast._
import encrywm.ast.TreeNodeScanner
import encrywm.builtins.Types._


object ComplexityAnalyzer{

  def scan(node: AST_NODE): Int = node match {
    case root: TREE_ROOT => scanRoot(root)
    case stmt: STMT => scanStmt(stmt)
    case expr: EXPR => scanExpr(expr)
    case _ => 0 // Do nothing or throw exception?.
  }

  private def scanRoot(root: TREE_ROOT): Int = root match {
    case c: TREE_ROOT.Contract => c.body.map(scan).sum
    case _ => 0 // Do nothing or throw exception?.
  }

  private def scanStmt(stmt: STMT): Int = stmt match {
    case STMT.FunctionDef(_, _, body, _) => 1 + body.map(scanStmt).sum
    case STMT.Return(value) => value.map(scanExpr).getOrElse(0)
    case STMT.Assign(_, value) => scanExpr(value)
    case STMT.AugAssign(_, _, value) => scanExpr(value)
    case STMT.For(_, _, body, orelse) => 1 + body.map(scanStmt).sum + orelse.map(scanStmt).sum
    case STMT.If(test, body, orelse) => 1 + scanExpr(test) + Math.max(body.map(scanStmt).sum, orelse.map(scanStmt).sum)
    case STMT.Assert(test, msg) => scanExpr(test) + msg.map(scanExpr).getOrElse(0)
    case STMT.Expr(value) => scanExpr(value)
    case STMT.Unlock => 1
    case STMT.Halt => 1
    case _ => 0
  }

  private def scanExpr(expr: EXPR): Int = expr match {
    case EXPR.BoolOp(_, values) => values.length
    case EXPR.BinOp(left, _, right, _) => scanExpr(left) + scanExpr(right) //6
    case EXPR.UnaryOp(_, operand, _) => scanExpr(operand)
    case EXPR.Lambda(_, body, _) => scanExpr(body)
    case EXPR.IfExp(test, body, orelse, _) => scanExpr(test) + Math.max(scanExpr(body), scanExpr(orelse))
    case EXPR.Compare(left, ops, comparators) => scanExpr(left) + ops.length + comparators.map(scanExpr).sum
    case EXPR.Call(func, args, _, _) => scanExpr(func) + args.map(scanExpr).sum
    case EXPR.IntConst(_) => 1
    case EXPR.LongConst(_) => 1
    case EXPR.FloatConst(_) => 1
    case EXPR.DoubleConst(_) => 1
    case EXPR.True => 1
    case EXPR.False => 1
    case EXPR.Str(_) => 1
    case EXPR.Base58Str(_) => 1
    case EXPR.Attribute(value, _, _, _) => scanExpr(value)
    case EXPR.Subscript(value, _, _, _) => scanExpr(value)
    case EXPR.Name(_, _, _) => 1
    case EXPR.Dict(keys, values, _) => keys.map(scanExpr).sum + values.map(scanExpr).sum
    case EXPR.ESet(elts, _) => elts.map(scanExpr).sum
    case EXPR.EList(elts, _, _) => elts.map(scanExpr).sum
    case EXPR.Tuple(elts, _, _) => elts.map(scanExpr).sum
    case EXPR.Decl(target, _) => scanExpr(target)
    case _ => 0 //Do nothing or throw exception?
  }


}
