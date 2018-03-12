package encrywm.parser

import encrywm.parser.Ast.EXPR
import encrywm.parser.Lexer.kwd
import encrywm.parser.WsApi._
import fastparse.core
import fastparse.noApi._

/**
  * Expression grammar. This is stuff that can be used within a larger
  * expression. Everything here ignores whitespace and does not care about
  * indentation
  */
object Expressions {

  val NAME: P[Ast.Identifier] = Lexer.identifier
  val NUMBER: P[Ast.EXPR.Num] = P( longConstExpr | intConstExpr )
  val STRING: P[String] = Lexer.stringliteral

  val intConstExpr: P[Ast.EXPR.IntConst] = P( Lexer.integer ).map(Ast.EXPR.IntConst)
  val longConstExpr: P[Ast.EXPR.LongConst] = P( Lexer.longinteger ).map(Ast.EXPR.LongConst)

  val test: P[Ast.EXPR] = {
    val ternary = P(orTest ~ (kwd("if") ~ orTest ~ kwd("else") ~ test).?).map {
      case (x, None) => x
      case (x, Some((t, neg))) => Ast.EXPR.IfExp(t, x, neg)
    }
    P(ternary | lambdef)
  }
  val orTest: core.Parser[Ast.EXPR, Char, String] = P( andTest.rep(1, kwd("or") | "||") ).map {
    case Seq(x) => x
    case xs => Ast.EXPR.BoolOp(Ast.BOOL_OP.Or, xs)
  }
  val andTest: core.Parser[Ast.EXPR, Char, String] = P( notTest.rep(1, kwd("and") | "&&") ).map {
    case Seq(x) => x
    case xs => Ast.EXPR.BoolOp(Ast.BOOL_OP.And, xs)
  }
  val notTest: P[Ast.EXPR] = P( (("not" | "!") ~ notTest).map(Ast.EXPR.UnaryOp(Ast.UNARY_OP.Not, _)) | comparison )

  val comparison: P[Ast.EXPR] = P( arith_expr ~ (comp_op ~ arith_expr).rep ).map {
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.EXPR.Compare(lhs, ops, vals)
  }

  // Common operators, mapped from their
  // strings to their type-safe representations
  def op[T](s: P0, rhs: T): core.Parser[T, Char, String] = s.!.map(_ => rhs)
  val Lt = op("<", Ast.COMP_OP.Lt)
  val Gt = op(">", Ast.COMP_OP.Gt)
  val Eq = op("==", Ast.COMP_OP.Eq)
  val GtE = op(">=", Ast.COMP_OP.GtE)
  val LtE = op("<=", Ast.COMP_OP.LtE)
  val NotEq = op("<>" | "!=", Ast.COMP_OP.NotEq)
  val In = op("in", Ast.COMP_OP.In)
  val NotIn = op("not" ~ "in", Ast.COMP_OP.NotIn)
  val Is = op("is", Ast.COMP_OP.Is)
  val IsNot = op("is" ~ "not", Ast.COMP_OP.IsNot)
  val comp_op = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )
  val Add = op("+", Ast.OPERATOR.Add)
  val Sub = op("-", Ast.OPERATOR.Sub)
  val Pow = op("**", Ast.OPERATOR.Pow)
  val Mult= op("*", Ast.OPERATOR.Mult)
  val Div = op("/", Ast.OPERATOR.Div)
  val Mod = op("%", Ast.OPERATOR.Mod)
  val FloorDiv = op("//", Ast.OPERATOR.FloorDiv)
  val UAdd = op("+", Ast.UNARY_OP.UAdd)
  val USub = op("-", Ast.UNARY_OP.USub)
  val Invert = op("~", Ast.UNARY_OP.Invert)
  val unary_op = P ( UAdd | USub | Invert )


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
  val term: P[Ast.EXPR] = P( Chain(factor, Mult | Div | Mod | FloorDiv) )
  // NUMBER appears here and below in `atom` to give it precedence.
  // This ensures that "-2" will parse as `Num(-2)` rather than
  // as `UnaryOp(USub, Num(2))`.
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
    val empty_tuple = ("(" ~ ")").map(_ => Ast.EXPR.Tuple(Nil, Ast.EXPR_CTX.Load))
    val empty_list = ("[" ~ "]").map(_ => Ast.EXPR.List(Nil, Ast.EXPR_CTX.Load))
    val empty_dict = ("{" ~ "}").map(_ => Ast.EXPR.Dict(Nil, Nil))
    P(
      empty_tuple  |
        empty_list |
        empty_dict |
        "(" ~ (tuple | test) ~ ")" |
        "[" ~ list ~ "]" |
        "{" ~ dictorsetmaker ~ "}" |
        STRING.rep(1).map(_.mkString).map(Ast.EXPR.Str) |
        NAME.map(Ast.EXPR.Name(_, Ast.EXPR_CTX.Load)) |
        NUMBER
    )
  }
  val listContents = P( test.rep(1, ",") ~ ",".? )
  val list = P( listContents ).map(Ast.EXPR.List(_, Ast.EXPR_CTX.Load))
  val tupleContents = P( test ~ "," ~ listContents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  val tuple = P( tupleContents).map(Ast.EXPR.Tuple(_, Ast.EXPR_CTX.Load))

  // TODO: Do we need lambdas?
  val lambdef: P[Ast.EXPR.Lambda] = P( kwd("lambda") ~ varargslist ~ ":" ~ test ).map(Ast.EXPR.Lambda.tupled )

  val trailer: P[Ast.EXPR => Ast.EXPR] = {
    val call = P("(" ~ arglist ~ ")").map { case (args, keywords) => (lhs: Ast.EXPR) => Ast.EXPR.Call(lhs, args, keywords) }
    val slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.EXPR) => Ast.EXPR.Subscript(lhs, args, Ast.EXPR_CTX.Load))
    val attr = P("." ~ NAME).map(id => (lhs: Ast.EXPR) => Ast.EXPR.Attribute(lhs, id, Ast.EXPR_CTX.Load))
    P(call | slice | attr)
  }

  val subscript: P[Ast.SLICE] = {
    val ellipses = P(("." ~ "." ~ ".").map(_ => Ast.SLICE.Ellipsis))
    val single = P(test.map(Ast.SLICE.Index))
    val multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      Ast.SLICE.Slice(
        lower,
        upper,
        step.map(_.getOrElse(Ast.EXPR.Name(Ast.Identifier("None"), Ast.EXPR_CTX.Load)))
      )
    }
    P( ellipses | multi | single )
  }

  val subscriptlist = P( subscript.rep(1, ",") ~ ",".? ).map {
    case Seq(x) => x
    case xs => Ast.SLICE.ExtSlice(xs)
  }

  val sliceop = P(":" ~ test.?)
  val exprlist: P[Seq[Ast.EXPR]] = P( expr.rep(1, sep = ",") ~ ",".? )
  val testlist: P[Seq[Ast.EXPR]] = P( test.rep(1, sep = ",") ~ ",".? )
  val dictorsetmaker: P[Ast.EXPR] = {
    val dict_item = P( test ~ ":" ~ test )
    val dict: P[Ast.EXPR.Dict] = P(
      (dict_item.rep(1, ",") ~ ",".?).map { x =>
        val (keys, values) = x.unzip
        Ast.EXPR.Dict(keys, values)
      }
    )

    val set: P[Ast.EXPR.Set] = P( test.rep(1, ",") ~ ",".? ).map(Ast.EXPR.Set)
    P( dict | set )
  }

  val plain_argument: core.Parser[EXPR, Char, String] = P( test )

  val named_argument: core.Parser[Ast.Keyword, Char, String] = P( NAME ~ "=" ~ test ).map(Ast.Keyword.tupled)

  val arglist = {
    val inits = P((plain_argument ~ !"=").rep(0, ","))
    val later = P(named_argument.rep(0, ","))
    P( inits ~ ",".? ~ later )
  }

  val testlist1: P[Seq[Ast.EXPR]] = P(test.rep(1, sep = ","))

  val varargslist: P[Ast.Arguments] = {
    val named_arg = P( fpdef )
    val x = P( (named_arg ~ Statements.typeDecl).rep(sep = ",") ).map(args => Ast.Arguments(args.map {
      case (a, t) => Ast.EXPR.Decl(a, Some(t)) }))
    P( x )
  }

  val fpdef: P[Ast.EXPR] = P(NAME.map(Ast.EXPR.Name(_, Ast.EXPR_CTX.Param)) | "(" ~ fplist ~ ")")
  val fplist: P[Ast.EXPR] = P(fpdef.rep(sep = ",") ~ ",".? ).map(Ast.EXPR.Tuple(_, Ast.EXPR_CTX.Param))

  def tuplize(exprs: Seq[Ast.EXPR]): Ast.EXPR = exprs match {
    case Seq(x) => x
    case xs => Ast.EXPR.Tuple(xs, Ast.EXPR_CTX.Load)
  }
}
