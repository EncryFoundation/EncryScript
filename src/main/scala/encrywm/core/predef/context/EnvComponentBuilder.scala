package encrywm.core.predef.context

import encrywm.backend.env.{ESObject, ESValue}

trait EnvComponentBuilder {
  def asVal: ESValue
  def build: ESObject
}
