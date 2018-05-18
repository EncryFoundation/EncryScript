package encrywm.lib.predef

import encrywm.lang.backend.env.ESBuiltInFunc

trait BuiltInFunctionHolder {
  val name: String
  def asFunc: ESBuiltInFunc
}
