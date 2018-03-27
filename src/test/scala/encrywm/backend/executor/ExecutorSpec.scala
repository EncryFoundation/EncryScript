package encrywm.backend.executor

import encrywm.ast.Ast.{AST_NODE, TREE_ROOT}
import encrywm.backend.executor.context.ScopedRuntimeContext
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.StaticAnalyser
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class ExecutorSpec extends PropSpec with Matchers {

  private def precess(s: String): AST_NODE = {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    StaticAnalyser.scan(parsed)
    parsed
  }

  private val exc = new Executor(ScopedRuntimeContext.initialized("GLOBAL", 1))

  property("Simple contract") {

    val tree = precess(
      """
        |let a = 999
        |let b = 9
        |a + b + 8
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isLeft shouldBe true
  }

  property("Contract with UnlockIf-stmt") {

    val tree = precess(
      """
        |let a = 30
        |let b = 30
        |
        |unlock if a >= b
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("Boolean operation in UnlockIf-stmt test (||)") {

    val tree = precess(
      """
        |let a = 10
        |let b = 30
        |
        |unlock if a >= b || true
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("Boolean operation in UnlockIf-stmt test (&&)") {

    val tree = precess(
      """
        |let a = 10
        |let b = 30
        |
        |unlock if a < b && true
      """.stripMargin)

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
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
        |
        |unlock if a <= sum(a, b)
      """.stripMargin)

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
        |unlock if a <= c
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("List subscription") {

    val tree = precess(
      """
        |let lst = [0, 1, 2, 3, 4]
        |let a: Int = lst[3]
        |
        |unlock if a >= lst[1]
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }

  property("Unary operation in test expr") {

    val tree = precess(
      """
        |unlock if not false
      """.stripMargin)

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
        |unlock if not checkSig(msg, sig, pk)
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.r.isInstanceOf[Executor.Unlocked.type] shouldBe true
  }
}
