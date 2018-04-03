package encrywm.lib.predef.env

import encrywm.backend.env.{ESObject, ESValue}
import encrywm.lib.Types.{ESProduct, ESType}

trait ESEnvConvertable {
  val esType: ESProduct
  def asVal: ESValue
  def convert: ESObject
}
