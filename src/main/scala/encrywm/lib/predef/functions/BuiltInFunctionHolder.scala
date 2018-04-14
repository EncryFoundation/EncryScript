package encrywm.lib.predef.functions

import encrywm.backend.env.ESBuiltInFunc

trait BuiltInFunctionHolder {
  val name: String
  def asFunc: ESBuiltInFunc
}
