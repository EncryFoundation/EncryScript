package encrywm.lang.frontend.parser

import encrywm.ast.Ast
import fastparse.all._
import fastparse.{all, core}

object  WsApi extends fastparse.WhitespaceApi.Wrapper(Lexer.wsComment)

object Lexer {

  def kwd(s: String): core.Parser[Unit, Char, String] = s ~ !(letter | digit | "_")

  val comment: all.Parser[Unit] =       P( "#" ~ CharsWhile(_ != '\n', min = 0) )
  val wsComment: all.Parser[Unit] =     P( (CharsWhileIn(" \n") | Lexer.comment | "\\\n").rep )
  val nnlWsComment: all.Parser[Unit] =  P( (CharsWhileIn(" ") | Lexer.comment | "\\\n").rep )

  val identifier: P[Ast.Identifier] =   P( (letter|"_") ~ (letter | digit | "_").rep ).!.filter(!keywords.contains(_))
    .map(Ast.Identifier)

  val letter: all.Parser[Unit] =        P( lowercase | uppercase )
  val lowercase: all.Parser[Unit] =     P( CharIn('a' to 'z') )
  val uppercase: all.Parser[Unit] =     P( CharIn('A' to 'Z') )
  val digit: all.Parser[Unit] =         P( CharIn('0' to '9') )

  val keywords = Set(
    "and",       "not",       "true",      "return",
    "elif",      "abort",     "or",
    "assert",    "else",      "if",
    "case",      "unlock",    "print",
    "match",     "exec",      "in",
    "let",       "is",        "false",
    "def",       "lamb",      "pass",
  )

  val stringliteral: P[String] = P( stringprefix.? ~ (longstring | shortstring) )
  val stringprefix: P0 = P(
    "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR"
  )
  val shortstring: P[String] = P( shortstring0("'") | shortstring0("\"") )
  def shortstring0(delimiter: String): all.Parser[String] = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P( shortstringchar(quote) | escapeseq )
  def shortstringchar(quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

  val longstring: P[String] = P( longstring0("'''") | longstring0("\"\"\"") )
  def longstring0(delimiter: String): all.Parser[String] = P( delimiter ~ longstringitem(delimiter).rep.! ~ delimiter)
  def longstringitem(quote: String): P0 = P( longstringchar(quote) | escapeseq | !quote ~ quote.take(1)  )
  def longstringchar(quote: String): P0 = P( CharsWhile(!s"\\${quote(0)}".contains(_)) )

  val escapeseq: P0 = P( "\\" ~ AnyChar )

  def negatable[T](p: P[T])(implicit ev: Numeric[T]): core.Parser[T, Char, String] = (("+" | "-").?.! ~ p).map {
    case ("-", i) => ev.negate(i)
    case (_, i) => i
  }

  val integer: core.Parser[Int, Char, String] = negatable[Int](P( CharIn('0' to '9').rep(min = 1).!.map(_.toInt) ))
  val longinteger: core.Parser[Long, Char, String] = negatable[Long](P( (integer ~ ("l" | "L").rep(max = 1, min = 1)).map(_.toLong) ))
  val floatinteger: core.Parser[Float, Char, String] = negatable[Float](P( (integer ~ "." ~ integer ~ ("f" | "F").rep(max = 1, min = 1)).!.map(_.toFloat)))
  val doubleinteger: core.Parser[Double, Char, String] = negatable[Double](P( (integer ~ "." ~ integer).!.map(_.toDouble)))

  val decimalinteger: P[BigInt] = P( nonzerodigit ~ digit.rep | "0" ).!.map(scala.BigInt(_))
  val octinteger: P[BigInt] = P( "0" ~ ("o" | "O") ~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).! ).map(scala.BigInt(_, 8))
  val hexinteger: P[BigInt] = P( "0" ~ ("x" | "X") ~ hexdigit.rep(1).! ).map(scala.BigInt(_, 16))
  val bininteger: P[BigInt] = P( "0" ~ ("b" | "B") ~ bindigit.rep(1).! ).map(scala.BigInt(_, 2))
  val nonzerodigit: P0 = P( CharIn('1' to '9') )
  val octdigit: P0 = P( CharIn('0' to '7') )
  val bindigit: P0 = P( "0" | "1" )
  val hexdigit: P0 = P( digit | CharIn('a' to 'f', 'A' to 'F') )

  val floatnumber: P[BigDecimal] = negatable[BigDecimal](P( pointfloat | exponentfloat ))
  val pointfloat: P[BigDecimal] = P( intpart.? ~ fraction | intpart ~ "." ).!.map(BigDecimal(_))
  val exponentfloat: P[BigDecimal] = P( (intpart | pointfloat) ~ exponent ).!.map(BigDecimal(_))
  val intpart: P[BigDecimal] = P( digit.rep(1) ).!.map(BigDecimal(_))
  val fraction: P0 = P( "." ~ digit.rep(1) )
  val exponent: P0 = P( ("e" | "E") ~ ("+" | "-").? ~ digit.rep(1) )

  val imagnumber: all.Parser[BigDecimal] = P( (floatnumber | intpart) ~ ("j" | "J") )

  val SchemaSeparator: String = "#---script---"
}
