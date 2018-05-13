package encrywm.lang.backend.env

import encrywm.lib.Types.ESProduct

trait ESEnvConvertable {
  val esType: ESProduct
  def asVal: ESValue
  def convert: ESObject
}
