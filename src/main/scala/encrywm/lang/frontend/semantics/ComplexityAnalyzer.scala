package encrywm.lang.frontend.semantics

import encrywm.ast.Ast._
import encrywm.lib.predef.PredefFunctions

import scala.collection.immutable.HashMap

object ComplexityAnalyzer {

  type ScriptComplexityScore = Int
  type FunctionName = String
  type ComplexityMap = HashMap[FunctionName, ScriptComplexityScore]

  def complexityOfScript(node: AST_NODE): ScriptComplexityScore = complexityOf(node)._1

  def complexityOf(node: AST_NODE, functionsComplexity: ComplexityMap = HashMap.empty): (ScriptComplexityScore, ComplexityMap) = node match {
    case root: TREE_ROOT => scanRoot(root, functionsComplexity)
    case stmt: STMT => scanStmt(stmt, functionsComplexity)
    case expr: EXPR => scanExpr(expr, functionsComplexity)
    case _ => 0 -> functionsComplexity
  }

  private def scanRoot(root: TREE_ROOT, functionsComplexity: ComplexityMap): (ScriptComplexityScore, ComplexityMap) = root match {
    case c: TREE_ROOT.Contract => c.body.foldLeft(0, HashMap[FunctionName, ScriptComplexityScore]()) {
      case (contractInfo, stmt) =>
        val stmtComplexityInfo = complexityOf(stmt, functionsComplexity)
        contractInfo._1 + stmtComplexityInfo._1 -> stmtComplexityInfo._2
    }
    case _ => 0 -> functionsComplexity
  }

  private def scanStmt(stmt: STMT, functionsComplexity: ComplexityMap): (ScriptComplexityScore, ComplexityMap) = stmt match {
    case STMT.FunctionDef(name, _, body, _) => 0 -> (functionsComplexity + (name.name -> body.map( scanStmt( _, functionsComplexity )._1).sum))
    case STMT.Return(value) => value.map(value => scanExpr(value, functionsComplexity)._1).getOrElse(0) -> functionsComplexity
    case STMT.Let(_, value, _) => scanExpr(value, functionsComplexity)._1 -> functionsComplexity
    case STMT.AugAssign(_, _, value) => scanExpr(value, functionsComplexity)._1 -> functionsComplexity
    case STMT.For(_, _, body, orelse) => 1 + body.map(scanStmt(_, functionsComplexity)._1).sum + orelse.map(scanStmt(_, functionsComplexity)._1).sum -> functionsComplexity
    case STMT.If(test, body, orelse) =>
      1 + scanExpr(test, functionsComplexity)._1 + Math.max(body.map(scanStmt(_, functionsComplexity)._1).sum, orelse.map(scanStmt(_, functionsComplexity)._1).sum) -> functionsComplexity
    case STMT.Assert(test, msg) => scanExpr(test, functionsComplexity)._1 + msg.map(scanExpr(_, functionsComplexity)._1).getOrElse(0) -> functionsComplexity
    case STMT.Expr(value) => scanExpr(value, functionsComplexity)
    case STMT.UnlockIf(expr) => scanExpr(expr, functionsComplexity)
    case STMT.Halt => 1 -> functionsComplexity
    case STMT.Pass => 1 -> functionsComplexity
    case _ => 0 -> functionsComplexity
  }

  // FIXME: User-defined function call complexity estimated incorrectly.
  private def scanExpr(expr: EXPR, functionsComplexity: ComplexityMap): (ScriptComplexityScore, ComplexityMap) = expr match {
    case EXPR.BoolOp(_, values) => values.length -> functionsComplexity
    case EXPR.BinOp(left, _, right, _) => scanExpr(left, functionsComplexity)._1 + scanExpr(right, functionsComplexity)._1 + 1 -> functionsComplexity
    case EXPR.UnaryOp(_, operand, _) => scanExpr(operand, functionsComplexity)
    case EXPR.Lambda(_, body, _) => scanExpr(body, functionsComplexity)
    case EXPR.IfExp(test, body, orelse, _) =>
      scanExpr(test, functionsComplexity)._1 + Math.max(scanExpr(body, functionsComplexity)._1, scanExpr(orelse, functionsComplexity)._1) -> functionsComplexity
    case EXPR.Compare(left, ops, comparators) =>
      scanExpr(left, functionsComplexity)._1 + ops.length + comparators.map(scanExpr(_, functionsComplexity)._1).sum -> functionsComplexity
    case EXPR.Call(EXPR.Name(Identifier(n), _, _), args, _, _) => args.map(scanExpr(_, functionsComplexity)._1).sum + {
      // TODO: Move complexity of predef function from here and replace 99999999
      if (PredefFunctions.hashFunctions.map(_.name).contains(n)) 10
      else if (PredefFunctions.middleFunctions.map(_.name).contains(n)) 15
      else if (PredefFunctions.heavyFunctions.map(_.name).contains(n)) 20
      else functionsComplexity.find(_._1 == n).map(_._2).getOrElse(99999999)
    } -> functionsComplexity
    case EXPR.IntConst(_) => 1 -> functionsComplexity
    case EXPR.LongConst(_) => 1 -> functionsComplexity
    case EXPR.True => 1 -> functionsComplexity
    case EXPR.False => 1 -> functionsComplexity
    case EXPR.Str(_) => 1 -> functionsComplexity
    case EXPR.Base58Str(_) => 1 -> functionsComplexity
    case EXPR.Attribute(value, _, _, _) => scanExpr(value, functionsComplexity)
    case EXPR.Subscript(value, _, _, _) => scanExpr(value, functionsComplexity)
    case EXPR.ESDictNode(keys, values, _) => keys.map(scanExpr(_, functionsComplexity)._1).sum + values.map(scanExpr(_, functionsComplexity)._1).sum -> functionsComplexity
    case EXPR.ESSet(elts, _) => elts.map(scanExpr(_, functionsComplexity)._1).sum -> functionsComplexity
    case EXPR.ESList(elts, _, _) => elts.map(scanExpr(_, functionsComplexity)._1).sum -> functionsComplexity
    case EXPR.ESTuple(elts, _, _) => elts.map(scanExpr(_, functionsComplexity)._1).sum -> functionsComplexity
    case EXPR.Declaration(target, _) => scanExpr(target, functionsComplexity)
    case EXPR.TypeMatching(_, tipe) => 2 + tipe.typeParams.length -> functionsComplexity
    case EXPR.GenericCond => 1 -> functionsComplexity
    case _ => 0 -> functionsComplexity
  }
}
