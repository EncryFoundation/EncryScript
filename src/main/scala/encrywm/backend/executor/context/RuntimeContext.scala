package encrywm.backend.executor.context

trait RuntimeContext {

  val types: Map[String, ESObject]
  val values: Map[String, ESValue]
  val functions: Map[String, ESFunc]
  val display: Map[String, Byte]

  def updated(s: Any): RuntimeContext

  def get(id: String): Option[ESCtxComponent] = display.get(id).flatMap {
    case ESObject.typeId => types.get(id)
    case ESValue.typeId => values.get(id)
    case ESFunc.typeId => functions.get(id)
  }
}
