package encrywm.backend.executor

import encrywm.backend.env.ScopedRuntimeEnv
import encrywm.backend.executor.env.{ESContextBuilder, ESPredefTestEnv, ESStateData, ESTransactionData}
import scorex.utils.Random

trait Execution {

  val testEnv: ESPredefTestEnv = {
    val transaction = ESTransactionData(Random.randomBytes(), Random.randomBytes(), Random.randomBytes(), 12345567L)
    val state = ESStateData(99999, 12345678L, Random.randomBytes())
    val context = new ESContextBuilder(state, transaction)

    new ESPredefTestEnv(context)
  }

  val exc: Executor = new Executor(ScopedRuntimeEnv.initialized("GLOBAL", 1, testEnv.predefMembers))
}
