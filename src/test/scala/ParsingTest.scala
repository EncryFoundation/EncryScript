import encrywm.parser.{Ast, Statements}
import org.scalatest.{Matchers, PropSpec}
import utils.ExprChecker
import fastparse.all._

class ParsingTest extends PropSpec with Matchers with ExprChecker {

  def stmt(expected: Seq[Ast.STMT], s: String*): Seq[Ast.STMT] =
    s.map(check(Statements.file_input, expected, _)).head

  property("Simple expression parsing (0)") {
    val source = "3 + 9 / 10"
    val parsed = (Statements.file_input ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Simple expression parsing (1)") {
    val source = "def main(a, b):\nprint a * b"
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
        |a = 4
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
        |sig = "5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9"
        |pk = "11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i"
        |checksig(ctx.transaction.bytes, proof.sig, ctx.transaction.pk)
      """.stripMargin
    val parsed = (Statements.file_input ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }
}
