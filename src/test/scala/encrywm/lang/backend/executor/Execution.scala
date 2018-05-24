package encrywm.lang.backend.executor

import encrywm.lang.backend.executor.Executor.{Return, Unlocked}
import encrywm.lang.backend.executor.env.{ESContextBuilder, ESStateData, ESTransactionData}
import encrywm.lib.TypeSystem
import scorex.utils.Random

trait Execution {

  def didUnlock(r: Executor.ExecOutcome): Boolean = r match {
    case Right(Return(_: Unlocked.type)) => true
    case _ => false
  }

  val testEnv: ESContextBuilder = {
    val transaction = ESTransactionData(Random.randomBytes(), Random.randomBytes(), Random.randomBytes(), 12345567L)
    val state = ESStateData(99999, 12345678L, Random.randomBytes())
    new ESContextBuilder(state, transaction)
  }

  val exc: Executor = Executor(testEnv.asVal, Int.MaxValue, TypeSystem.default)
}
