package encrywm.frontend.parser

import encrywm.frontend.ast.Ast
import encrywm.frontend.parser.Expressions._
import encrywm.frontend.parser.Lexer.kwd
import encrywm.frontend.parser.WsApi._
import fastparse.noApi
import fastparse.noApi._

object Statements extends Statements(0)

/**
  * Statement grammar. This can only be used in statement-blocks,
  * and is sensitive to newlines and indentation to determine nesting
  */
class Statements(indent: Int){

  val SPACE: noApi.Parser[Unit] = P( CharIn(" \n") )
  val NEWLINE: P0 = P( "\n" | End )
  val ENDMARKER: P0 = P( End )

  val single_input: P[Seq[Ast.STMT]] = P(
    NEWLINE.map(_ => Nil) | simpleStmt | compoundStmt.map(Seq(_)) ~ NEWLINE
  )

  val indents = P( "\n" ~~ " ".repX(indent) )

  val spaces = P( (Lexer.nnlWsComment.? ~~ "\n").repX(1) )
  val fileInput: P[Seq[Ast.STMT]] = P( spaces.? ~ stmt.repX(0, spaces) ~ spaces.? ).map(_.flatten)
  val evalInput: P[Ast.EXPR] = P( testlist ~ NEWLINE.rep ~ ENDMARKER ).map(tuplize)

  val contract: P[Ast.TREE_ROOT.Contract] = P( fileInput ).map(stmts => Ast.TREE_ROOT.Contract(stmts))

  def collapse_dotted_name(name: Seq[Ast.Identifier]): Ast.EXPR = {
    name.tail.foldLeft[Ast.EXPR](Ast.EXPR.Name(name.head, Ast.EXPR_CTX.Load))(
      (x, y) => Ast.EXPR.Attribute(x, y, Ast.EXPR_CTX.Load)
    )
  }

  val retTypeDecl: P[Ast.Identifier] = P( "->" ~/ NAME )
  val funcDef: P[Ast.STMT.FunctionDef] = P( kwd("def") ~/ NAME ~ fnParameters ~/ retTypeDecl ~ ":" ~~ block ).map {
    case (name, args, tpe, blc) => Ast.STMT.FunctionDef(name, args, blc, tpe)
  }

  val typeDecl: P[Ast.Identifier] = P( ":" ~ NAME )

  val fnParameters: P[Ast.Arguments] = P( "(" ~ varargslist ~ ")" )

  val stmt: P[Seq[Ast.STMT]] = P( compoundStmt.map(Seq(_)) | simpleStmt )

  val simpleStmt: P[Seq[Ast.STMT]] = P( smallStmt.rep(1, sep = ";") ~ ";".? )
  val smallStmt: P[Ast.STMT] = P( flowStmt | assertStmt | exprStmt)

  val exprStmt: P[Ast.STMT] = {
    val aug = P( testlist ~ augassign ~ test )
    val tstl = P( testlist )
    val let = P( kwd("let") ~/ NAME ~ typeDecl.? ~ ("=" ~ test) )

    P(
      aug.map { case (a, b, c) => Ast.STMT.AugAssign(tuplize(a), b, c) } |
        tstl.map(a => Ast.STMT.Expr(tuplize(a))) |
        let.map {
          case (a, t, b) => Ast.STMT.Assign(Ast.EXPR.Decl(Ast.EXPR.Name(a, Ast.EXPR_CTX.Store), t), b)
        }
    )
  }

  val augassign: P[Ast.OPERATOR] = P(
     "+=".!.map(_ => Ast.OPERATOR.Add) |
      "-=".!.map(_ => Ast.OPERATOR.Sub) |
      "*=".!.map(_ => Ast.OPERATOR.Mult) |
      "/=".!.map(_ => Ast.OPERATOR.Div) |
      "%=".!.map(_ => Ast.OPERATOR.Mod) |
      "**=".!.map(_ => Ast.OPERATOR.Pow) |
      "//=".!.map(_ => Ast.OPERATOR.FloorDiv)
  )

  val flowStmt: P[Ast.STMT] = P( returnStmt | abortStmt | unlockStmt )

  // Those statements are under discussion now.
  val unlockStmt = P(kwd("unlock") ).map(_ => Ast.STMT.Unlock)
  val abortStmt = P(kwd("abort") ).map(_ => Ast.STMT.Halt)

  val returnStmt = P(kwd("return") ~~ " ".rep ~~ testlist.map(tuplize).? ).map(Ast.STMT.Return)


  val dotted_as_name: P[Ast.Alias] = P( dotted_name.map(x => Ast.Identifier(x.map(_.name).mkString("."))) ~ (kwd("as") ~ NAME).? )
    .map(Ast.Alias.tupled)
  val dotted_as_names = P( dotted_as_name.rep(1, ",") )
  val dotted_name = P( NAME.rep(1, ".") )

  val assertStmt: P[Ast.STMT.Assert] = P( kwd("assert") ~ test ~ ("," ~ test).? ).map(Ast.STMT.Assert.tupled)

  val compoundStmt: P[Ast.STMT] = P( ifStmt | forStmt | funcDef )
  val ifStmt: P[Ast.STMT.If] = {
    val firstIf = P( kwd("if") ~/ test ~ ":" ~~ block )
    val elifs = P( (spaceIndents ~~ kwd("elif") ~/ test ~ ":" ~~ block).repX )
    val lastElse = P( (spaceIndents ~~ kwd("else") ~/ ":" ~~ block).? )
    P( firstIf ~~ elifs ~~ lastElse ).map {
      case (test, body, elifs, orelse) =>
        val (init :+ last) = (test, body) +: elifs
        val (last_test, last_body) = last
        init.foldRight(Ast.STMT.If(last_test, last_body, orelse.toSeq.flatten)){
          case ((t, b), rhs) => Ast.STMT.If(t, b, Seq(rhs))
        }
    }
  }
  val spaceIndents: P0 = P( spaces.repX ~~ " ".repX(indent) )

  val forStmt: P[Ast.STMT.For] = P( kwd("for") ~/ exprlist ~ kwd("in") ~ testlist ~ ":" ~~ block
    ~~ (spaceIndents ~ kwd("else") ~/ ":" ~~ block).? ).map {
      case (itervars, generator, body, orelse) =>
        Ast.STMT.For(tuplize(itervars), tuplize(generator), body, orelse.toSeq.flatten)
    }

  val block: P[Seq[Ast.STMT]] = {
    val deeper: P[Int] = {
      val commentLine = P( "\n" ~~ Lexer.nnlWsComment.?.map(_ => 0) ).map((_, Some("")))
      val endLine = P( "\n" ~~ (" " | "\t").repX(indent + 1).!.map(_.length) ~~ Lexer.comment.!.? )
      P( Lexer.nnlWsComment.? ~~ ( endLine | commentLine ).repX(1) ).map {
        _.collectFirst{ case (s, None) => s }
      }.filter(_.isDefined).map(_.get)
    }
    val indented = P( deeper.flatMap { nextIndent =>
      new Statements(nextIndent).stmt.repX(1, spaces.repX(1) ~~ (" " * nextIndent | "\t" * nextIndent)).map(_.flatten)
    } )
    P( indented | " ".rep ~ simpleStmt )
  }
}
