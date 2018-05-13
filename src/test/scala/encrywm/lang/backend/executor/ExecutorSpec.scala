package encrywm.lang.backend.executor

import org.scalatest.{Matchers, PropSpec}

class ExecutorSpec extends PropSpec with Matchers with Execution {

  import encrywm.common.SourceProcessor._

  property("Simple contract") {

    val tree = process(
      """
        |let a = 999
        |let b = 9
        |a + b + 8
      """.stripMargin)

    val excR = exc.executeContract(tree.get)
    excR.isRight shouldBe true

    excR.right.get.isInstanceOf[Executor.Nothing.type] shouldBe true
  }

  property("Contract with UnlockIf-stmt") {

    val tree = process(
      """
        |let a = 30
        |let b = 30
        |
        |unlock if a >= b
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Boolean operation in UnlockIf-stmt test (||)") {

    val tree = process(
      """
        |let a = 10
        |let b = 30
        |
        |unlock if a >= b || true
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    excR.isRight shouldBe true

    didUnlock(excR) shouldBe true
  }

  property("Boolean operation in UnlockIf-stmt test (&&)") {

    val tree = process(
      """
        |let a = 10
        |let b = 30
        |
        |unlock if a < b && true
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    excR.isRight shouldBe true

    didUnlock(excR) shouldBe true
  }

  property("Contract with fn call in If-stmt test") {

    val tree = process(
      """
        |let a = 10
        |let b = 30
        |
        |def sum(a: Int, b: Int) -> Int:
        |    return a + b
        |
        |unlock if a <= sum(a, b)
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Return stmt in function body") {

    val tree = process(
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

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("If-expression in assignment") {

    val tree = process(
      """
        |let a = 0
        |let b = 30
        |
        |let c = 100 if a < 30 else -50
        |
        |unlock if a <= c
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("List subscription") {

    val tree = process(
      """
        |let lst = [0, 1, 2, 3, 4]
        |let a = lst[3]
        |
        |unlock if a.get >= 0
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Dict subscription") {

    val tree = process(
      """
        |let map = {"2" : 2, "1" : 1}
        |let a = map["2"]
        |
        |unlock if a.get >= 1
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Object attribute reference") {

    val tree = process(
      """
        |let a: Long = context.state.height
        |
        |unlock if a >= 100
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Unary operation in test expr") {

    val tree = process(
      """
        |unlock if not false
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("SizeOf list") {

    val tree = process(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |unlock if coll.size > 2
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Sum list") {

    val tree = process(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |unlock if coll.sum > 5
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("list.Exists(lambda) (true case)") {

    val tree = process(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |unlock if coll.exists(lamb (i: Int) = i == 2)
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("list.Exists(func) (true case)") {

    val tree = process(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |def equals2(n: Int) -> Bool:
        |    return n == 2
        |
        |unlock if coll.exists(equals2)
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("list.Exists(predicate) (false case)") {

    val tree = process(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |unlock if not coll.exists(lamb (i: Int) = i == 9)
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("list.Map(func)") {

    val tree = process(
      """
        |let coll = [1, 2, 3, 4, 5]
        |
        |let mapped = coll.map(lamb (n: Int) = n * 1000)
        |
        |unlock if mapped.sum > 10000
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Match statement") {

    val tree = process(
      """
        |let a = true
        |
        |match a:
        |    case true:
        |        unlock if true
        |    case _:
        |        unlock if false
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Match statement (Default branch execution)") {

    val tree = process(
      """
        |let a = true
        |
        |match a:
        |    case true:
        |        unlock if true
        |    case _:
        |        abort
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Global value declaration") {

    val tree = process(
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

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Executor scoping (Referencing from inner scope)") {

    val tree = process(
      """
        |let a = true
        |
        |if true:
        |    unlock if a
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("BuiltIn function (CheckSig)") {

    val tree = process(
      """
        |let msg = base58"11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox"
        |let sig = base58"FRQ91MwL3MV3LVEG8Ej3ZspTLgUJqSLtcHM66Zk11xY1"
        |let pk = base58"117gRnfiknXThwHF6fb4A8WQdgNxA6ZDxYApqu7MztH"
        |
        |unlock if not checkSig(sig, msg, pk)
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("BuiltIn function (Base58Decode)") {

    val tree = process(
      """
        |let litDec = base58"11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox"
        |let fnDec = decode("11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox")
        |
        |unlock if litDec == fnDec.get
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe true
  }

  property("Max coll size overflow") {

    val tree = process(
      """
        |let coll = [1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5,
        |            5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4,
        |            1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5]
        |
        |let mapped = coll.map(lamb (n: Int) = n * 1000)
        |
        |unlock if mapped.sum > 10000
      """.stripMargin)

    val excR = exc.executeContract(tree.get)

    didUnlock(excR) shouldBe false
  }
}
