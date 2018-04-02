package encrywm.frontend.parser

import encrywm.ast.Ast
import encrywm.frontend.parser.Statements
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}
import utils.ExprChecker

class ParsingTest extends PropSpec with Matchers with ExprChecker {

  def stmt(expected: Seq[Ast.STMT], s: String*): Seq[Ast.STMT] =
    s.map(check(Statements.fileInput, expected, _)).head

  property("Simple expression") {
    val source = "3 + 9"
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Long int") {
    val source =
      """
        |let a = 13l
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Double int") {
    val source =
      """
        |let a = 13.44
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Float int") {
    val source =
      """
        |let a = 12.45f
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Base58 string") {
    val source =
      """
        |let a = base58"5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9"
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Function definition") {
    val source =
      """
        |def func(a: int, b: int) -> int:
        |  a + b + c
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Assignment") {
    val source =
      """
        |let a: str = 'string'
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Variable referencing") {
    val source =
      """
        |let c = a + b * 8
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Function referencing") {
    val source =
      """
        |let c = 8 + func()
        |main()
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Dictionary") {
    val source =
      """
        |let d = {'Alice' : 100,
        |         'Bob' : 1000,
        |         'Tom' : 50}
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("List") {
    val source =
      """
        |let l = ['Tom', 'Bob', 'Alice']
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Conditional assignment") {
    val source =
      """
        |let a: long = 1 if true and true else 0
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Dot-notation") {
    val source =
      """
        |let a = obj.attr0.attr1
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Subscription") {
    val source =
      """
        |let a = list[0]
        |let b = list[:2]
        |let c = list[4:]
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("If statement with compound test") {
    val source =
      """
        |if true and true or 3 >= 8:
        |    return 100 * 100 * a
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Unlock If statement") {
    val source =
      """
        |unlock if true
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Unlock If statement with compound test expr") {
    val source =
      """
        |unlock if true and 6 > 8 and call()
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Match statement (Simple expr in branch)") {
    val source =
      """
        |let a = 100
        |match a:
        |   case 100:
        |       1 + 1
        |   case 200:
        |       0 * 1000
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Match statement (Type matching in branch)") {
    val source =
      """
        |match proof:
        |   case sig -> Signature25519:
        |       checkSig(sig)
        |   case _:
        |       0 * 1000
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    println(parsed)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }

  property("Complex expression") {
    val source =
      """
        |let publicKeys = {'Ivan' : pkFromAddress('5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9'),
        |                  'Marina' : pkFromAddress('11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i'),
        |                  'Alice': pkFromAddress('75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p')}
        |
        |if checkType(proof, MultiProof) and proof.proofs.size >= 2:
        |    let flag1 = 1 if proof.proofs['Ivan'].isDefined and checkSig(ctx.transaction.bytes, proof.proofs['Ivan'], publicKeys[0]) else 0
        |    let flag2 = 1 if proof.proofs['Marina'].isDefined and checkSig(ctx.transaction.bytes, proof.proofs['Marina'], publicKeys[1]) else 0
        |    let flag3 = 1 if proof.proofs['Alice'].isDefined and checkSig(ctx.transaction.bytes, proof.proofs['Alice'], publicKeys[2]) else 0
        |
        |    unlock if (flag1 + flag2 + flag3) >= 2
      """.stripMargin
    val parsed = (Statements.fileInput ~ End).parse(source)

    parsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true
  }
}
