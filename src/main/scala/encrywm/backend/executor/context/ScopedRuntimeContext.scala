package encrywm.backend.executor.context

class ScopedRuntimeContext(val name: String,
                           val level: Int,
                           override val members: Map[String, ESRuntimeComponent],
                           parentOpt: Option[ScopedRuntimeContext] = None) extends RuntimeContext {

  def updated(s: ESRuntimeComponent): ScopedRuntimeContext =
    new ScopedRuntimeContext(name, level, members.updated(s.name, s), parentOpt)

  def emptyChild(n: String): ScopedRuntimeContext = ScopedRuntimeContext.empty(n, level + 1)

  override def get(id: String): Option[ESRuntimeComponent] = super.get(id) match {
    case None => parentOpt.flatMap(_.get(id))
    case Some(r) => Some(r)
  }

  override def toString: String = s"<ScopedContext name=$name lvl=$level size=${members.size}>"
}

object ScopedRuntimeContext {

  def apply(name: String,
            level: Int,
            members: Map[String, ESRuntimeComponent] = Map.empty,
            parentOpt: Option[ScopedRuntimeContext] = None): ScopedRuntimeContext =
    new ScopedRuntimeContext(name, level, members, parentOpt)

  def empty(n: String, l: Int): ScopedRuntimeContext =
    new ScopedRuntimeContext(n, l, Map.empty, None)

  def initialized(n: String, l: Int, ctx: ESPredefContext): ScopedRuntimeContext =
    new ScopedRuntimeContext(n, l, ctx.predefMembers, None)
}
