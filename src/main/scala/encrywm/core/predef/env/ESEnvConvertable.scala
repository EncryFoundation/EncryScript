package encrywm.core.predef.env

import encrywm.backend.env.{ESObject, ESValue}
import encrywm.core.Types.{ESProduct, ESType}

trait ESEnvConvertable {
  val esType: ESProduct
  def asVal: ESValue
  def convert: ESObject
}
