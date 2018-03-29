package encrywm.core.environment.functions

import encrywm.backend.env.ESBuiltInFunc

trait BuiltInFunctionHolder {
  def asFunc: ESBuiltInFunc
}
