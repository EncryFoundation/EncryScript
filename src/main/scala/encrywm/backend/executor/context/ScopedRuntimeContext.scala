package encrywm.backend.executor.context

import encrywm.builtins.functions.CheckSig

class ScopedRuntimeContext(val name: String,
                           val level: Int,
                           override val types: Map[String, ESObject],
                           override val values: Map[String, ESValue],
                           override val functions: Map[String, ESFunc],
                           override val biFunctions: Map[String, ESBuiltInFunc],
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
        biFunctions,
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
        biFunctions,
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
        biFunctions,
        display.updated(f.name, ESFunc.typeId),
        parentOpt
      )
    case biF: ESBuiltInFunc =>
      new ScopedRuntimeContext(
        name,
        level,
        types,
        values,
        functions,
        biFunctions.updated(biF.name, biF),
        display.updated(biF.name, ESBuiltInFunc.typeId),
        parentOpt
      )
  }

  def emptyChild(n: String): ScopedRuntimeContext = ScopedRuntimeContext.empty(n, level + 1)

  override def get(id: String): Option[ESCtxComponent] = super.get(id) match {
    case None => parentOpt.flatMap(_.get(id))
    case Some(r) => Some(r)
  }

  override def toString: String = s"<ScopedContext name=$name lvl=$level size=${display.size}>"
}

object ScopedRuntimeContext {

  def apply(name: String,
            level: Int,
            types: Map[String, ESObject] = Map.empty,
            values: Map[String, ESValue] = Map.empty,
            functions: Map[String, ESFunc] = Map.empty,
            biFunctions: Map[String, ESBuiltInFunc] = Map.empty,
            display: Map[String, Byte] = Map.empty,
            parentOpt: Option[ScopedRuntimeContext] = None): ScopedRuntimeContext =
    new ScopedRuntimeContext(name, level, types, values, functions, biFunctions, display, parentOpt)

  def empty(n: String, l: Int): ScopedRuntimeContext =
    new ScopedRuntimeContext(n, l, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None)

  def initialized(n: String, l: Int): ScopedRuntimeContext = {
      new ScopedRuntimeContext(n, l, Map.empty, Map.empty, Map.empty,
        Map(CheckSig.name -> CheckSig.fn), Map(CheckSig.name -> ESBuiltInFunc.typeId), None)
    }
}
