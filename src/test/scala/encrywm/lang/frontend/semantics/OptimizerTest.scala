package encrywm.lang.frontend.semantics

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.lang.ESCompiler
import encrywm.lang.backend.executor.Execution
import org.scalatest.{Matchers, PropSpec}

class OptimizerTest extends PropSpec with Matchers with Execution {

  property("Simple AST optimization") {

    def sample(i: Int) = s"let longName$i = 999\n"

    val tree = ESCompiler.compile((0 to 100).map(sample).reduce(_ + _))

    val optimized = new Optimizer().optimize(tree.get)

    val execR = exc.executeContract(optimized.asInstanceOf[Contract])

    execR.isRight shouldBe true
  }

  property("Complex AST optimization") {

    val source =
      """
        |let minobrPk = base58'5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9'
        |let universityPk = base58'11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i'
        |let studentPk = base58'75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p'
        |
        |let myAssetId = base58'GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew'
        |
        |def isMyAsset(box: Box) -> Bool:
        |    match box:
        |        case asset -> AssetBox:
        |            return asset.tokenIdOpt.isDefined && asset.tokenIdOpt.get == myAssetId
        |        case _:
        |            return false
        |
        |def validThresholdSig(proof: Proof) -> Bool:
        |    match proof:
        |        case mulp -> MultiSig:
        |            let sig1 = 1 if mulp.proofs[0].isDefined && checkSig(mulp.proofs[0].get.sigBytes, context.transaction.messageToSign, minobrPk) else 0
        |            let sig2 = 1 if mulp.proofs[1].isDefined && checkSig(mulp.proofs[1].get.sigBytes, context.transaction.messageToSign, universityPk) else 0
        |            let sig3 = 1 if mulp.proofs[2].isDefined && checkSig(mulp.proofs[2].get.sigBytes, context.transaction.messageToSign, studentPk) else 0
        |            return (sig1 + sig2 + sig3) >= 2    # threshold sig 2 of 3
        |        case _:
        |            return false
      """.stripMargin

    val tree = ESCompiler.compile(source)

    val optimized = new Optimizer().optimize(tree.get)

    val execR = exc.executeContract(optimized.asInstanceOf[Contract])

    execR.isRight shouldBe true
  }
}
