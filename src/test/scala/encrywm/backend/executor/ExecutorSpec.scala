package encrywm.backend.executor

import encrywm.ast.Ast.{AST_NODE, TREE_ROOT}
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.StaticAnalyser
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class ExecutorSpec extends PropSpec with Matchers {

  def precess(s: String): AST_NODE = {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    StaticAnalyser.scan(parsed)
    parsed
  }

  property("Simple contract") {

    val tree = precess(
      """
        |let a = 999
        |let b = 9
        |a + b + 8
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isLeft shouldBe true
  }

  property("Contract with If-stmt") {

    val tree = precess(
      """
        |let a = 30
        |let b = 30
        |if a >= b:
        |    unlock
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("Contract with 2 branch If-stmt") {

    val tree = precess(
      """
        |let a = 10
        |let b = 30
        |if a >= b:
        |    unlock
        |else:
        |    abort
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Halt.type] shouldBe true
  }

  property("Boolean operation in If-stmt test (||)") {

    val tree = precess(
      """
        |let a = 10
        |let b = 30
        |if a >= b || true:
        |    unlock
        |else:
        |    abort
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("Boolean operation in If-stmt test (&&)") {

    val tree = precess(
      """
        |let a = 10
        |let b = 30
        |if a < b && true:
        |    unlock
        |else:
        |    abort
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }


  property("Contract with fn call in If-stmt test") {

    val tree = precess(
      """
        |let a = 10
        |let b = 30
        |
        |def sum(a: int, b: int) -> int:
        |    return a + b
        |
        |if a <= sum(a, b):
        |    unlock
        |else:
        |    abort
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("If-expression in assignment") {

    val tree = precess(
      """
        |let a = 0
        |let b = 30
        |
        |let c = 100 if a < 30 else -50
        |
        |if a <= c:
        |    unlock
        |else:
        |    abort
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("List subscription") {

    val tree = precess(
      """
        |let lst = [0, 1, 2, 3, 4]
        |let a: int = lst[3]
        |
        |if a >= lst[1]:
        |    unlock
        |else:
        |    abort
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("BuiltIn function") {

    val tree = precess(
      """
        |let msg = base58"11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox"
        |let sig = base58"FRQ91MwL3MV3LVEG8Ej3ZspTLgUJqSLtcHM66Zk11xY1"
        |let pk = base58"117gRnfiknXThwHF6fb4A8WQdgNxA6ZDxYApqu7MztH"
        |
        |if checkSig(msg, sig, pk):
        |    unlock
        |else:
        |    abort
      """.stripMargin)

    val exc = new Executor

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Halt.type] shouldBe true
  }
}
