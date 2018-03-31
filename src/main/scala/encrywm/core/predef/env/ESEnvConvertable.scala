package encrywm.core.predef.env

import encrywm.backend.env.{ESObject, ESValue}

trait ESEnvConvertable {
  def asVal: ESValue
  def convert: ESObject
}
