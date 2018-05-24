package encrywm.ast

import encrywm.ast.Ast._

object AstStringifier {

  def toString(node: AST_NODE): String = node match {
    case root: TREE_ROOT => rootToString(root)
    case stmt: STMT => stmtToString(stmt)
    case expr: EXPR => exprToString(expr)
    case _ => " "
  }

  private def rootToString(root: TREE_ROOT): String = root match {
    case c: TREE_ROOT.Contract => c.body.map(toString).fold("")(_.concat(_))
    case c: TREE_ROOT.Expression => c.body.map(toString).fold("")(_.concat(_))
  }

  private def stmtToString(stmt: STMT): String = stmt match {
    case STMT.FunctionDef(name, args, body, returnType) =>
      s"def ${name.name}(" +
        s"${args.args.head._1.name}: ${args.args.head._2.ident.name}" +
        s"${args.args.drop(1).map(arg => s"${arg._1.name}: ${arg._2.ident.name}").fold("")(_ + ", " + _)}): -> ${returnType.name}:"
        .concat(body.map(toString).fold("")((str, expr) => str.concat("\n" +  expr)))
    case STMT.Return(value) => "return " + value.map(toString(_)).getOrElse("")
    case STMT.Let(target, value, _) => s"let ".concat(toString(target)).concat(" = ").concat(toString(value)) + "\n"
    case STMT.AugAssign(_, _, _) => "<+=>"
    case STMT.For(_, _, _, _) => "<for_stmt>"
    case STMT.If(_, _, _) => "<if_stmt>"
    case STMT.Match(target, branches) => s"match ${toString(target)}: \n" + branches.foldLeft("")((resultStr, branch) => resultStr + toString(branch))
    case STMT.Case(cond, body, _) => s"case ${toString(cond)}: \n" + body.foldLeft("")((str, stmt) => str.concat("  " + toString(stmt) + "\n"))
    case STMT.Assert(_, _) => "<assert_stmt>"
    case STMT.Expr(value) => toString(value)
    case STMT.UnlockIf(expr) => "unlock if ".concat(toString(expr))
    case STMT.Halt => "abort"
    case STMT.Pass => "pass"
  }

  private def exprToString(expr: EXPR): String = expr match {
    case EXPR.BoolOp(op, values) => values.tail.foldLeft(toString(values.head))((str, expr) => str.concat(s" ${boolOpToString(op)} ${toString(expr)}"))
    case EXPR.BinOp(left, op, right, _) => s"${toString(left)} ${binOpToString(op)} ${toString(right)}"
    case EXPR.UnaryOp(op, operand, _) => unaryOpToString(op) + toString(operand)
    case EXPR.Lambda(args, body, _) => "lamb (" + args.args.foldLeft("")((str, ident) => str + s"${ident._1}: ${ident._2.ident.name}") + ") = " + toString(body)
    case EXPR.IfExp(test, body, orelse, _) => s"${toString(body)} if ${toString(test)} else ${toString(orelse)}"
    case EXPR.Compare(left, ops, comparators) => toString(left).concat(
      ops.map(compOpToString).zip(comparators.map(toString)).foldLeft("")((str, elem) => str.concat(s" ${elem._1} ${elem._2}"))
    )
    case EXPR.Call(func, args, _, _) => toString(func) + "(" + args.tail.foldLeft(toString(args.head))((str, expr) => str.concat(", " + toString(expr))) + ")"
    case EXPR.IntConst(const) => const.toString
    case EXPR.LongConst(const) => const.toString
    case EXPR.True => true.toString
    case EXPR.False => false.toString
    case EXPR.Str(str) => "\"" + str + "\""
    case EXPR.Base58Str(str) => s"base58{$str}"
    case EXPR.Attribute(value, attr, _, _) => s"${toString(value)}.${attr.name}"
    case EXPR.Subscript(_, _, _, _) => "<subscript_expr>"
    case EXPR.Name(name, _, _) => name.name
    case EXPR.ESDictNode(keys, values, _) => keys.zip(values).foldLeft("")( (str, elem) => str.concat(toString(elem._1) + ":" + toString(elem._2)))
    case EXPR.ESSet(elts, _) => elts.foldLeft("")((str, expr) => str.concat(toString(expr) + ", "))
    case EXPR.ESList(elts, _, _) => "[" + elts.drop(1).foldLeft(toString(elts.head))((str, expr) => str.concat(", " + toString(expr))) + "]"
    case EXPR.ESTuple(elts, _, _) => "(" + elts.drop(1).foldLeft(toString(elts.head))((str, expr) => str.concat(", " + toString(expr))) + ")"
    case EXPR.Declaration(target, tpe) => toString(target) + tpe.map(": " + _.ident.name).getOrElse("")
    case EXPR.GenericCond => "_"
  }
  
  private def compOpToString(op: COMP_OP): String = op match {
    case Ast.COMP_OP.Eq => "=="
    case Ast.COMP_OP.NotEq => "<>"
    case Ast.COMP_OP.Lt => "<"
    case Ast.COMP_OP.LtE => "<="
    case Ast.COMP_OP.Gt => ">"
    case Ast.COMP_OP.GtE => ">="
    case Ast.COMP_OP.Is => "is"
    case Ast.COMP_OP.IsNot => "is not"
    case Ast.COMP_OP.In => "in"
    case Ast.COMP_OP.NotIn => "not in"
  }

  private def boolOpToString(op: BOOL_OP): String = op match {
    case Ast.BOOL_OP.And => "&&"
    case Ast.BOOL_OP.Or => "||"
  }
  
  private def binOpToString(op: OPERATOR): String = op match {
    case Ast.OPERATOR.Add => "+"
    case Ast.OPERATOR.Sub => "-"
    case Ast.OPERATOR.Mult => "*"
    case Ast.OPERATOR.Div => "/"
    case Ast.OPERATOR.Mod => "%"
  }

  private def unaryOpToString(op: UNARY_OP): String = op match {
    case Ast.UNARY_OP.Invert => "~"
    case Ast.UNARY_OP.Not => "!"
    case Ast.UNARY_OP.UAdd => "+"
    case Ast.UNARY_OP.USub => "-"
  }
}
