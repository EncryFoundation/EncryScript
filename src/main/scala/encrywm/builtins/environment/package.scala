package encrywm.builtins

import encrywm.builtins.environment.context.ESTransaction
import encrywm.builtins.environment.functions.CheckSig

package object environment {

  val components: Seq[ESEnvComponentHolder] = Seq(ESTransaction, CheckSig)
}
