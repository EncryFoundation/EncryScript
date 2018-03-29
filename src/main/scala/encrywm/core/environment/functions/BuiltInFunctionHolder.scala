package encrywm.core.environment.functions

import encrywm.backend.env.ESBuiltInFunc
import encrywm.core.environment.ESEnvComponent

trait BuiltInFunctionHolder {
  def asFunc: ESBuiltInFunc
}
