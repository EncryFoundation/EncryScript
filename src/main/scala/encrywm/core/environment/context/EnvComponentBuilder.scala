package encrywm.core.environment.context

import encrywm.backend.env.{ESObject, ESValue}

trait EnvComponentBuilder {
  def asVal: ESValue
  def build: ESObject
}
