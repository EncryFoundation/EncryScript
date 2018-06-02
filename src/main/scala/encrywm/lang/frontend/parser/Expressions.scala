package encrywm.lang.frontend.parser

import encrywm.ast.Ast
import encrywm.ast.Ast.{COMP_OP, EXPR, OPERATOR, UNARY_OP}
import encrywm.lang.frontend.parser.Lexer.kwd
import encrywm.lang.frontend.parser.WsApi._
import fastparse.noApi._
import fastparse.{core, noApi}

/**
  * Expression grammar. This is stuff that can be used within a larger
  * expression. Everything here ignores whitespace and does not care about
  * indentation
  */
object Expressions {

  val NAME: P[Ast.Identifier] = Lexer.identifier
  val NUMBER: P[Ast.EXPR.Num] = P( longConstExpr| intConstExpr )
  val BOOL: P[Ast.EXPR.Bool] = P( trueExpr | falseExpr )
  val STRING: P[String] = Lexer.stringliteral
  val BASE58STRING: P[String] = P( "base58" ~/ Lexer.stringliteral )

  val trueExpr: P[Ast.EXPR.True.type] = P( kwd("true").rep(min = 1, max = 1).! ).map(_ => Ast.EXPR.True)
  val falseExpr: P[Ast.EXPR.False.type] = P( kwd("false").rep(min = 1, max = 1).! ).map(_ => Ast.EXPR.False)

  val intConstExpr: P[Ast.EXPR.IntConst] = P( Lexer.integer ).map(Ast.EXPR.IntConst)
  val longConstExpr: P[Ast.EXPR.LongConst] = P( Lexer.longinteger ).map(Ast.EXPR.LongConst)

  val test: P[Ast.EXPR] = {
    val ternary = P(orTest ~ (kwd("if") ~ orTest ~ kwd("else") ~ test).?).map {
      case (x, None) => x
      case (x, Some((t, neg))) => Ast.EXPR.IfExp(t, x, neg)
    }
    P( ternary | lambdef )
  }
  val orTest: core.Parser[Ast.EXPR, Char, String] = P( andTest.rep(1, kwd("or") | "||") ).map {
    case Seq(x) => x
    case xs => Ast.EXPR.BoolOp(Ast.BOOL_OP.Or, xs.toList)
  }
  val andTest: core.Parser[Ast.EXPR, Char, String] = P( notTest.rep(1, kwd("and") | "&&") ).map {
    case Seq(x) => x
    case xs => Ast.EXPR.BoolOp(Ast.BOOL_OP.And, xs.toList)
  }
  val notTest: P[Ast.EXPR] = P( (("not" | "!") ~ notTest).map(Ast.EXPR.UnaryOp(Ast.UNARY_OP.Not, _)) | comparison )

  val comparison: P[Ast.EXPR] = P( arith_expr ~ (comp_op ~ arith_expr).rep ).map {
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.EXPR.Compare(lhs, ops.toList, vals.toList)
  }

  // Common operators, mapped from their
  // strings to their type-safe representations
  def op[T](s: P0, rhs: T): core.Parser[T, Char, String] = s.!.map(_ => rhs)
  val Lt: core.Parser[COMP_OP.Lt.type, Char, String] = op("<", Ast.COMP_OP.Lt)
  val Gt: core.Parser[COMP_OP.Gt.type, Char, String] = op(">", Ast.COMP_OP.Gt)
  val Eq: core.Parser[COMP_OP.Eq.type, Char, String] = op("==", Ast.COMP_OP.Eq)
  val GtE: core.Parser[COMP_OP.GtE.type, Char, String] = op(">=", Ast.COMP_OP.GtE)
  val LtE: core.Parser[COMP_OP.LtE.type, Char, String] = op("<=", Ast.COMP_OP.LtE)
  val NotEq: core.Parser[COMP_OP.NotEq.type, Char, String] = op("<>" | "!=", Ast.COMP_OP.NotEq)
  val In: core.Parser[COMP_OP.In.type, Char, String] = op("in", Ast.COMP_OP.In)
  val NotIn: core.Parser[COMP_OP.NotIn.type, Char, String] = op("not" ~ "in", Ast.COMP_OP.NotIn)
  val Is: core.Parser[COMP_OP.Is.type, Char, String] = op("is", Ast.COMP_OP.Is)
  val IsNot: core.Parser[COMP_OP.IsNot.type, Char, String] = op("is" ~ "not", Ast.COMP_OP.IsNot)
  val comp_op: noApi.Parser[Ast.COMP_OP with Product with Serializable] = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )
  val Add: core.Parser[OPERATOR.Add.type, Char, String] = op("+", Ast.OPERATOR.Add)
  val Sub: core.Parser[OPERATOR.Sub.type, Char, String] = op("-", Ast.OPERATOR.Sub)
  val Pow: core.Parser[OPERATOR.Pow.type, Char, String] = op("**", Ast.OPERATOR.Pow)
  val Mult: core.Parser[OPERATOR.Mult.type, Char, String] = op("*", Ast.OPERATOR.Mult)
  val Div: core.Parser[OPERATOR.Div.type, Char, String] = op("/", Ast.OPERATOR.Div)
  val Mod: core.Parser[OPERATOR.Mod.type, Char, String] = op("%", Ast.OPERATOR.Mod)
  val UAdd: core.Parser[UNARY_OP.UAdd.type, Char, String] = op("+", Ast.UNARY_OP.UAdd)
  val USub: core.Parser[UNARY_OP.USub.type, Char, String] = op("-", Ast.UNARY_OP.USub)
  val Invert: core.Parser[UNARY_OP.Invert.type, Char, String] = op("~", Ast.UNARY_OP.Invert)
  val unary_op: noApi.Parser[Ast.UNARY_OP with Product with Serializable] = P ( UAdd | USub | Invert )


  def Unary(p: P[Ast.EXPR]): core.Parser[EXPR.UnaryOp, Char, String] =
    (unary_op ~ p).map { case (op, operand) => Ast.EXPR.UnaryOp(op, operand) }

  def Chain(p: P[Ast.EXPR], op: P[Ast.OPERATOR]): core.Parser[EXPR, Char, String] =
    P( p ~ (op ~ p).rep ).map {
      case (lhs, chunks) =>
        chunks.foldLeft(lhs) { case (lhs, (op, rhs)) =>
          Ast.EXPR.BinOp(lhs, op, rhs)
        }
    }

  val expr: P[Ast.EXPR] = P( arith_expr )
  val arith_expr: P[Ast.EXPR] = P( Chain(term, Add | Sub) )
  val term: P[Ast.EXPR] = P( Chain(factor, Mult | Div | Mod) )

  /**
    * NUMBER appears here and below in `atom` to give it precedence.
    * This ensures that "-2" will parse as `Num(-2)` rather than
    * as `UnaryOp(USub, Num(2))`.
    */
  val factor: P[Ast.EXPR] = P( NUMBER | Unary(factor) | power )
  val power: P[Ast.EXPR] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map {
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.EXPR.BinOp(left, op, right)
      }
  }
  val atom: P[Ast.EXPR] = {
    val empty_tuple = ("(" ~ ")").map(_ => Ast.EXPR.ESTuple(Nil))
    val empty_list = ("[" ~ "]").map(_ => Ast.EXPR.ESList(Nil))
    val empty_dict = ("{" ~ "}").map(_ => Ast.EXPR.ESDictNode(Nil, Nil))
    P(
      empty_tuple  |
        empty_list |
        empty_dict |
        "(" ~ (tuple | test) ~ ")" |
        "[" ~ list ~ "]" |
        "{" ~ dictorsetmaker ~ "}" |
        BASE58STRING.rep(1).map(_.mkString).map(Ast.EXPR.Base58Str) |
        STRING.rep(1).map(_.mkString).map(Ast.EXPR.Str) |
        NAME.map(Ast.EXPR.Name(_)) |
        NUMBER |
        BOOL
    )
  }
  val listContents: noApi.Parser[Seq[EXPR]] = P( test.rep(1, ",") ~ ",".? )
  val list: core.Parser[EXPR.ESList, Char, String] = P( listContents ).map(exps => Ast.EXPR.ESList(exps.toList))
  val tupleContents: core.Parser[Seq[EXPR], Char, String] = P( test ~ "," ~ listContents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  val tuple: core.Parser[EXPR.ESTuple, Char, String] = P( tupleContents ).map(tcs => Ast.EXPR.ESTuple(tcs.toList))

  val lambdef: P[Ast.EXPR.Lambda] = P( kwd("lamb") ~ "(" ~ varargslist ~ ")" ~ "=" ~ test ).map { case (args, exp) => Ast.EXPR.Lambda(args, exp) }

  val trailer: P[Ast.EXPR => Ast.EXPR] = {
    val call = P("(" ~ arglist ~ ")").map { case (args, keywords) => (lhs: Ast.EXPR) => Ast.EXPR.Call(lhs, args.toList, keywords.toList) }
    val slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.EXPR) => Ast.EXPR.Subscript(lhs, args))
    val attr = P("." ~ NAME).map(id => (lhs: Ast.EXPR) => Ast.EXPR.Attribute(lhs, id))
    P(call | slice | attr)
  }

  val subscript: P[Ast.SLICE] = {
    val ellipses = P(("." ~ "." ~ ".").map(_ => Ast.SLICE.Ellipsis))
    val single = P(test.map(Ast.SLICE.Index))
    val multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      Ast.SLICE.Slice(
        lower,
        upper,
        step.map(_.getOrElse(Ast.EXPR.Name(Ast.Identifier("None"))))
      )
    }
    P( ellipses | multi | single )
  }

  val subscriptlist: core.Parser[Ast.SLICE, Char, String] = P( subscript.rep(1, ",") ~ ",".? ).map {
    case Seq(x) => x
    case xs => Ast.SLICE.ExtSlice(xs.toList)
  }

  val sliceop: noApi.Parser[Option[EXPR]] = P(":" ~ test.?)
  val exprlist: P[Seq[Ast.EXPR]] = P( expr.rep(1, sep = ",") ~ ",".? )
  val testlist: P[Seq[Ast.EXPR]] = P( test.rep(1, sep = ",") ~ ",".? )
  val dictorsetmaker: P[Ast.EXPR] = {
    val dict_item = P( test ~ ":" ~ test )
    val dict: P[Ast.EXPR.ESDictNode] = P(
      (dict_item.rep(1, ",") ~ ",".?).map { x =>
        val (keys, values) = x.unzip
        Ast.EXPR.ESDictNode(keys.toList, values.toList)
      }
    )

    val set: P[Ast.EXPR.ESSet] = P( test.rep(1, ",") ~ ",".? ).map(exp => Ast.EXPR.ESSet(exp.toList))
    P( dict | set )
  }

  val typeMatching: P[Ast.EXPR.TypeMatching] = P( NAME ~ Statements.typeDeclarationArrow ).map { case (name, tpe) =>
    Ast.EXPR.TypeMatching(name, tpe)
  }

  val schemaMatching: P[Ast.EXPR.SchemaMatching] = P( NAME ~ Statements.schemaDeclarationArrow ).map { case (name, schemaId) =>
    Ast.EXPR.SchemaMatching(name, schemaId)
  }

  val genericCond: P[Ast.EXPR.GenericCond.type] = P( "_" ).map(_ => Ast.EXPR.GenericCond)

  val plain_argument: core.Parser[EXPR, Char, String] = P( test )

  val named_argument: core.Parser[Ast.Keyword, Char, String] = P( NAME ~ "=" ~ test ).map(Ast.Keyword.tupled)

  val arglist: noApi.Parser[(Seq[EXPR], Seq[Ast.Keyword])] = {
    val inits = P((plain_argument ~ !"=").rep(0, ","))
    val later = P(named_argument.rep(0, ","))
    P( inits ~ ",".? ~ later )
  }

  val testlist1: P[Seq[Ast.EXPR]] = P(test.rep(1, sep = ","))

  val varargslist: P[Ast.Arguments] = {
    val named_arg = P( fpdef )
    val x = P( (named_arg ~/ Statements.typeDeclarationSemi).rep(sep = ",") ).map(args => Ast.Arguments(args.toList.map {
      case (EXPR.Name(id, _), tId) => id -> tId }))
    P( x )
  }

  val fpdef: P[Ast.EXPR] = P(NAME.map(Ast.EXPR.Name(_)) | "(" ~ fplist ~ ")")
  val fplist: P[Ast.EXPR] = P(fpdef.rep(sep = ",") ~ ",".? ).map(defs => Ast.EXPR.ESTuple(defs.toList))

  def tuplize(exprs: Seq[Ast.EXPR]): Ast.EXPR = exprs match {
    case Seq(x) => x
    case xs => Ast.EXPR.ESTuple(xs.toList)
  }
}
