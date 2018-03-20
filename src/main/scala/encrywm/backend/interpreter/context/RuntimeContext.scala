package encrywm.backend.interpreter.context

class RuntimeContext(val types: Map[String, ESObject],
                     val values: Map[String, ESValue],
                     val functions: Map[String, ESFunc],
                     private val display: Map[String, Byte]) {

  def updated(s: Any): RuntimeContext = s match {
    case o: ESObject =>
      new RuntimeContext(types.updated(o.name, o), values, functions, display.updated(o.name, ESObject.typeId))
    case v: ESValue =>
      new RuntimeContext(types, values.updated(v.name, v), functions, display.updated(v.name, ESValue.typeId))
    case f: ESFunc =>
      new RuntimeContext(types, values, functions.updated(f.name, f), display.updated(f.name, ESFunc.typeId))
  }

  def get(id: String): Option[ESCtxComponent] = display.get(id).flatMap {
    case ESObject.typeId => types.get(id)
    case ESValue.typeId => values.get(id)
    case ESFunc.typeId => functions.get(id)
  }
}
