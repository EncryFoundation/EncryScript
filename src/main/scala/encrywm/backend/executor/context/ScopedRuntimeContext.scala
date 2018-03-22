package encrywm.backend.executor.context

class ScopedRuntimeContext(val name: String,
                           val level: Int,
                           override val types: Map[String, ESObject],
                           override val values: Map[String, ESValue],
                           override val functions: Map[String, ESFunc],
                           override val display: Map[String, Byte],
                           parentOpt: Option[ScopedRuntimeContext] = None) extends RuntimeContext {

  def updated(s: Any): ScopedRuntimeContext = s match {
    case o: ESObject =>
      new ScopedRuntimeContext(
        name,
        level,
        types.updated(o.name, o),
        values,
        functions,
        display.updated(o.name, ESObject.typeId),
        parentOpt
      )
    case v: ESValue =>
      new ScopedRuntimeContext(
        name,
        level,
        types,
        values.updated(v.name, v),
        functions,
        display.updated(v.name, ESValue.typeId),
        parentOpt
      )
    case f: ESFunc =>
      new ScopedRuntimeContext(
        name,
        level,
        types,
        values,
        functions.updated(f.name, f),
        display.updated(f.name, ESFunc.typeId),
        parentOpt
      )
  }

  def emptyChild(n: String): ScopedRuntimeContext = ScopedRuntimeContext.empty(n, level + 1)

  override def get(id: String): Option[ESCtxComponent] = super.get(id) match {
    case None => parentOpt.flatMap(_.get(id))
    case Some(r) => Some(r)
  }
}

object ScopedRuntimeContext {

  def empty(n: String, l: Int): ScopedRuntimeContext =
    new ScopedRuntimeContext(n, l, Map.empty, Map.empty, Map.empty, Map.empty, None)
}
