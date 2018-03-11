package parser

import encrywm.parser.Ast
import encrywm.parser.Statements
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}
import utils.ExprChecker

class ParsingTest extends PropSpec with Matchers with ExprChecker {

  def stmt(expected: Seq[Ast.STMT], s: String*): Seq[Ast.STMT] =
    s.map(check(Statements.file_input, expected, _)).head

  property("Simple expression parsing") {
    val source = "3 + 9 / 10"
    val parsed = (Statements.file_input ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Complex expression parsing") {
    val source =
      """
        |def main(a, b) -> unit:
        | let a: int = 900 / 8 if a > b && a == b else 0
        | let b = 0
        | print a + b
        |
        |main(1, b=9)
      """.stripMargin
    val parsed = (Statements.file_input ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Invalid simple expression parsing") {
    val source = "def main:\npass"
    val parsed = (Statements.file_input ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Failure] shouldBe true
  }

  property("Complicated expression parsing (1)") {
    val source =
      """
        |let a: int = 4
        |if a < 5:
        |  if a >= 1:
        |    print false
        |  print a
        |else:
        |  print "string"
      """.stripMargin
    val parsed = (Statements.file_input ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Special expression parsing") {
    val source =
      """
        |let p: int = 9000
        |let s: bool = True if p < 900 and p == 0 else False
        |let sig: str = "5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9"
        |let pk: str = "11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i"
        |checksig(ctx.transaction.bytes, proof.sig, ctx.transaction.pk)
      """.stripMargin
    val parsed = (Statements.file_input ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }
}
