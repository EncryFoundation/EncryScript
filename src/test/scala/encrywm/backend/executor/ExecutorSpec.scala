package encrywm.backend.executor

import encrywm.ast.Ast.TREE_ROOT
import org.scalatest.{Matchers, PropSpec}
import utils.SourceProcessor

class ExecutorSpec extends PropSpec with Matchers with SourceProcessor with Execution {

  property("Simple contract") {

    val tree = precess(
      """
        |let a = 999
        |let b = 9
        |a + b + 8
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    excR.isRight shouldBe true

    excR.right.get.isInstanceOf[Executor.Nothing.type] shouldBe true
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

    didUnlock(excR) shouldBe true
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

    didUnlock(excR) shouldBe true
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

    didUnlock(excR) shouldBe true
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

    didUnlock(excR) shouldBe true
  }

  property("Return stmt in function body") {

    val tree = precess(
      """
        |let a = 10
        |let b = 30
        |
        |def fn(a: Int, b: Int) -> Bool:
        |    return a < b
        |    abort
        |
        |unlock if fn(a, b)
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
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

    didUnlock(excR) shouldBe true
  }

  property("List subscription") {

    val tree = precess(
      """
        |let lst = [0, 1, 2, 3, 4]
        |let a = lst[3]
        |
        |unlock if a.get >= 0
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("Dict subscription") {

    val tree = precess(
      """
        |let map = {"2" : 2, "1" : 1}
        |let a = map["2"]
        |
        |unlock if a.get >= 1
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("Object attribute reference") {

    val tree = precess(
      """
        |let a: Long = context.state.height
        |
        |unlock if a >= 100
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("Unary operation in test expr") {

    val tree = precess(
      """
        |unlock if not false
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("SizeOf list") {

    val tree = precess(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |unlock if coll.size > 2
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("list.Exists(predicate) (true case)") {

    val tree = precess(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |unlock if coll.exists(lamb (i: Int) = i == 2)
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("list.Exists(predicate) (false case)") {

    val tree = precess(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |unlock if not coll.exists(lamb (i: Int) = i == 9)
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("Match statement") {

    val tree = precess(
      """
        |let a = true
        |
        |match a:
        |    case true:
        |        unlock if true
        |    case _:
        |        unlock if false
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("Match statement (Default branch execution)") {

    val tree = precess(
      """
        |let a = true
        |
        |match a:
        |    case true:
        |        unlock if true
        |    case _:
        |        abort
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("Global value declaration") {

    val tree = precess(
      """
        |let a = true
        |
        |match a:
        |    case true:
        |        global let unlockFlag = true
        |    case _:
        |        pass
        |
        |unlock if unlockFlag
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("Executor scoping (Referencing from inner scope)") {

    val tree = precess(
      """
        |let a = true
        |
        |if true:
        |    unlock if a
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }

  property("BuiltIn function") {

    val tree = precess(
      """
        |let msg = base58"11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox"
        |let sig = base58"FRQ91MwL3MV3LVEG8Ej3ZspTLgUJqSLtcHM66Zk11xY1"
        |let pk = base58"117gRnfiknXThwHF6fb4A8WQdgNxA6ZDxYApqu7MztH"
        |
        |unlock if not checkSig(sig, msg, pk)
      """.stripMargin)

    val excR = exc.executeContract(tree.asInstanceOf[TREE_ROOT.Contract])

    didUnlock(excR) shouldBe true
  }
}
