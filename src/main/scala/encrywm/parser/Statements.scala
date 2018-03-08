package encrywm.parser

import fastparse.noApi._
import WsApi._
import Expressions._
import Scanner.kwd

object Statements extends Statements(0)

/**
  * Python's statement grammar. This can only be used in statement-blocks,
  * and is sensitive to newlines and indentation to determine nesting
  *
  * Manually transcribed from https://docs.python.org/2/reference/grammar.html
  */
class Statements(indent: Int){

  val space = P( CharIn(" \n") )
  val NEWLINE: P0 = P( "\n" | End )
  val ENDMARKER: P0 = P( End )

  val single_input: P[Seq[Ast.STMT]] = P(
    NEWLINE.map(_ => Nil) |
      simple_stmt |
      compound_stmt.map(Seq(_)) ~ NEWLINE
  )

  val indents = P( "\n" ~~ " ".repX(indent) )

  val spaces = P( (Scanner.nnlWsComment.? ~~ "\n").repX(1) )
  val file_input: P[Seq[Ast.STMT]] = P( spaces.? ~ stmt.repX(0, spaces) ~ spaces.? ).map(_.flatten)
  val eval_input: P[Ast.EXPR] = P( testlist ~ NEWLINE.rep ~ ENDMARKER ).map(tuplize)

  def collapse_dotted_name(name: Seq[Ast.Identifier]): Ast.EXPR = {
    name.tail.foldLeft[Ast.EXPR](Ast.EXPR.Name(name.head, Ast.EXPR_CTX.Load))(
      (x, y) => Ast.EXPR.Attribute(x, y, Ast.EXPR_CTX.Load)
    )
  }

  val decorator: P[Ast.EXPR] = P( "@" ~/ dotted_name ~ ("(" ~ arglist ~ ")" ).?  ~~ Scanner.nnlWsComment.? ~~ NEWLINE).map{
    case (name, None) => collapse_dotted_name(name)
    case (name, Some((args, (keywords, starargs, kwargs)))) =>
      val x = collapse_dotted_name(name)
      Ast.EXPR.Call(x, args, keywords, starargs, kwargs)
  }

  val decorators = P( decorator.rep )
  val decorated: P[Ast.STMT] = P( decorators ~ (classdef | funcdef) ).map{case (a, b) => b(a)}
  val classdef: P[Seq[Ast.EXPR] => Ast.STMT.ClassDef] =
    P( kwd("class") ~/ NAME ~ ("(" ~ testlist.? ~ ")").?.map(_.toSeq.flatten.flatten) ~ ":" ~~ suite ).map{
      case (a, b, c) => Ast.STMT.ClassDef(a, b, c, _)
    }


  val funcdef: P[Seq[Ast.EXPR] => Ast.STMT.FunctionDef] = P( kwd("def") ~/ NAME ~ parameters ~ ":" ~~ suite ).map{
    case (name, args, suite) => Ast.STMT.FunctionDef(name, args, suite, _)
  }
  val parameters: P[Ast.arguments] = P( "(" ~ varargslist ~ ")" )

  val stmt: P[Seq[Ast.STMT]] = P( compound_stmt.map(Seq(_)) | simple_stmt )

  val simple_stmt: P[Seq[Ast.STMT]] = P( small_stmt.rep(1, sep = ";") ~ ";".? )
  val small_stmt: P[Ast.STMT] = P(
    print_stmt  | del_stmt | pass_stmt | flow_stmt |
      import_stmt | global_stmt | exec_stmt | assert_stmt | expr_stmt
  )
  val expr_stmt: P[Ast.STMT] = {
    val aug = P( testlist ~ augassign ~ (yield_expr | testlist.map(tuplize)) )
    val assign = P( testlist ~ ("=" ~ (yield_expr | testlist.map(tuplize))).rep )

    P(
      aug.map{case (a, b, c) => Ast.STMT.AugAssign(tuplize(a), b, c) } |
        assign.map{
          case (a, Nil) => Ast.STMT.Expr(tuplize(a))
          case (a, b) => Ast.STMT.Assign(Seq(tuplize(a)) ++ b.init, b.last)
        }
    )
  }

  val augassign: P[Ast.OPERATOR] = P(
    "+=".!.map(_ => Ast.OPERATOR.Add) |
      "-=".!.map(_ => Ast.OPERATOR.Sub) |
      "*=".!.map(_ => Ast.OPERATOR.Mult) |
      "/=".!.map(_ => Ast.OPERATOR.Div) |
      "%=".!.map(_ => Ast.OPERATOR.Mod) |
      "&=".!.map(_ => Ast.OPERATOR.BitAnd) |
      "|=".!.map(_ => Ast.OPERATOR.BitOr) |
      "^=".!.map(_ => Ast.OPERATOR.BitXor) |
      "<<=".!.map(_ => Ast.OPERATOR.LShift) |
      ">>=".!.map(_ => Ast.OPERATOR.RShift) |
      "**=".!.map(_ => Ast.OPERATOR.Pow) |
      "//=".!.map(_ => Ast.OPERATOR.FloorDiv)
  )

  val print_stmt: P[Ast.STMT.Print] = {
    val noDest = P( test.rep(sep = ",") ~ ",".?).map(Ast.STMT.Print(None, _, true))
    val dest = P( ">>" ~ test ~ ("," ~ test).rep ~ ",".?).map{case (dest, exprs) => Ast.STMT.Print(Some(dest), exprs, true)}
    P( "print" ~~ " ".rep ~~ (noDest | dest) )
  }
  val del_stmt = P( kwd("del") ~~ " ".rep ~~ exprlist ).map(Ast.STMT.Delete)
  val pass_stmt = P( kwd("pass") ).map(_ => Ast.STMT.Pass)
  val flow_stmt: P[Ast.STMT] = P( break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt )
  val break_stmt = P( kwd("break") ).map(_ => Ast.STMT.Break)
  val continue_stmt = P( kwd("continue") ).map(_ => Ast.STMT.Continue)
  val return_stmt = P( kwd("return") ~~ " ".rep ~~ testlist.map(tuplize).? ).map(Ast.STMT.Return)

  val yield_stmt = P( yield_expr ).map(Ast.STMT.Expr)
  val raise_stmt: P[Ast.STMT.Raise] = P( kwd("raise") ~~ " ".rep ~~test.? ~ ("," ~ test).? ~ ("," ~ test).? ).map(Ast.STMT.Raise.tupled)
  val import_stmt: P[Ast.STMT] = P( import_name | import_from )
  val import_name: P[Ast.STMT.Import] = P( kwd("import") ~ dotted_as_names ).map(Ast.STMT.Import)
  val import_from: P[Ast.STMT.ImportFrom] = {
    val named = P( ".".rep(1).!.? ~ dotted_name.!.map(Some(_)) )
    val unNamed = P( ".".rep(1).!.map(x => (Some(x), None)) )
    val star = P( "*".!.map(_ => Seq(Ast.alias(Ast.Identifier("*"), None))) )
    P( kwd("from") ~ (named | unNamed) ~ kwd("import") ~ (star | "(" ~ import_as_names ~ ")" | import_as_names) ).map{
      case (dots, module, names) => Ast.STMT.ImportFrom(module.map(Ast.Identifier), names, dots.map(_.length))
    }
  }
  val import_as_name: P[Ast.alias] = P( NAME ~ (kwd("as") ~ NAME).? ).map(Ast.alias.tupled)
  val dotted_as_name: P[Ast.alias] = P( dotted_name.map(x => Ast.Identifier(x.map(_.name).mkString("."))) ~ (kwd("as") ~ NAME).? ).map(Ast.alias.tupled)
  val import_as_names = P( import_as_name.rep(1, ",") ~ (",").? )
  val dotted_as_names = P( dotted_as_name.rep(1, ",") )
  val dotted_name = P( NAME.rep(1, ".") )
  val global_stmt: P[Ast.STMT.Global] = P( kwd("global") ~ NAME.rep(sep = ",") ).map(Ast.STMT.Global)
  val exec_stmt: P[Ast.STMT.Exec] = P( kwd("exec") ~ expr ~ (kwd("in") ~ test ~ ("," ~ test).?).? ).map {
    case (expr, None) => Ast.STMT.Exec(expr, None, None)
    case (expr, Some((globals, None))) => Ast.STMT.Exec(expr, Some(globals), None)
    case (expr, Some((globals, Some(locals)))) => Ast.STMT.Exec(expr, Some(globals), Some(locals))
  }
  val assert_stmt: P[Ast.STMT.Assert] = P( kwd("assert") ~ test ~ ("," ~ test).? ).map(Ast.STMT.Assert.tupled)

  val compound_stmt: P[Ast.STMT] = P( if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | decorated )
  val if_stmt: P[Ast.STMT.If] = {
    val firstIf = P( kwd("if") ~/ test ~ ":" ~~ suite )
    val elifs = P( (space_indents ~~ kwd("elif") ~/ test ~ ":" ~~ suite).repX )
    val lastElse = P( (space_indents ~~ kwd("else") ~/ ":" ~~ suite).? )
    P( firstIf ~~ elifs ~~ lastElse ).map{
      case (test, body, elifs, orelse) =>
        val (init :+ last) = (test, body) +: elifs
        val (last_test, last_body) = last
        init.foldRight(Ast.STMT.If(last_test, last_body, orelse.toSeq.flatten)){
          case ((test, body), rhs) => Ast.STMT.If(test, body, Seq(rhs))
        }
    }
  }
  val space_indents = P( spaces.repX ~~ " ".repX(indent) )
  val while_stmt = P( kwd("while") ~/ test ~ ":" ~~ suite ~~ (space_indents ~~ kwd("else") ~/ ":" ~~ suite).?.map(_.toSeq.flatten) ).map(Ast.STMT.While.tupled)
  val for_stmt: P[Ast.STMT.For] = P( kwd("for") ~/ exprlist ~ kwd("in") ~ testlist ~ ":" ~~ suite ~~ (space_indents ~ kwd("else") ~/ ":" ~~ suite).? ).map {
    case (itervars, generator, body, orelse) =>
      Ast.STMT.For(tuplize(itervars), tuplize(generator), body, orelse.toSeq.flatten)
  }
  val try_stmt: P[Ast.STMT]= {
    val `try` = P( kwd("try") ~/ ":" ~~ suite )
    val excepts: P[Seq[Ast.EXCP_HANDLER]] = P( (except_clause ~ ":" ~~ suite).map{
      case (None, body) => Ast.EXCP_HANDLER.ExceptHandler(None, None, body)
      case (Some((x, None)), body) => Ast.EXCP_HANDLER.ExceptHandler(Some(x), None, body)
      case (Some((x, Some(y))), body) => Ast.EXCP_HANDLER.ExceptHandler(Some(x), Some(y), body)
    }.repX )
    val `else` = P( space_indents ~~ kwd("else") ~/ ":" ~~ suite )
    val `finally` = P( space_indents ~~ kwd("finally") ~/ ":" ~~ suite )
    P( `try` ~~ excepts ~~ `else`.? ~~ `finally`.? ).map{
      case (tryBlock, excepts, elseBlock, None) =>
        Ast.STMT.TryExcept(tryBlock, excepts, elseBlock.toSeq.flatten)
      case (tryBlock, Nil, None, Some(finallyBlock)) =>
        Ast.STMT.TryFinally(tryBlock, finallyBlock)
      case (tryBlock, excepts, elseBlock, Some(finallyBlock)) =>
        Ast.STMT.TryFinally(
          Seq(Ast.STMT.TryExcept(tryBlock, excepts, elseBlock.toSeq.flatten)),
          finallyBlock
        )
    }
  }
  val with_stmt: P[Ast.STMT.With] = P( kwd("with") ~/ with_item.rep(1, ",")~ ":" ~~ suite ).map{
    case (items, body) =>
      val (last_expr, last_vars) = items.last
      val inner = Ast.STMT.With(last_expr, last_vars, body)
      items.init.foldRight(inner){
        case ((expr, vars), body) => Ast.STMT.With(expr, vars, Seq(body))
      }
  }
  val with_item: P[(Ast.EXPR, Option[Ast.EXPR])] = P( test ~ (kwd("as") ~ expr).? )
  // NB compile.c makes sure that the default except clause is last
  val except_clause = P( space_indents ~ kwd("except") ~/ (test ~ ((kwd("as") | ",") ~ test).?).? )


  val suite: P[Seq[Ast.STMT]] = {
    val deeper: P[Int] = {
      val commentLine = P("\n" ~~ Scanner.nnlWsComment.?.map(_ => 0)).map((_, Some("")))
      val endLine = P("\n" ~~ (" "|"\t").repX(indent + 1).!.map(_.length) ~~ Scanner.comment.!.? )
      P( Scanner.nnlWsComment.? ~~ ( endLine | commentLine ).repX(1) ).map{
        _.collectFirst{ case (s, None) => s}
      }.filter(_.isDefined).map(_.get)
    }
    val indented = P( deeper.flatMap{ nextIndent =>
      new Statements(nextIndent).stmt.repX(1, spaces.repX(1) ~~ (" " * nextIndent | "\t" * nextIndent)).map(_.flatten)
    } )
    P( indented | " ".rep ~ simple_stmt )
  }
}
