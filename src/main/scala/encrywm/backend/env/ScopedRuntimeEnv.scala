package encrywm.backend.env

class ScopedRuntimeEnv(val name: String,
                       val level: Int,
                       override val members: Map[String, ESEnvComponent],
                       parentOpt: Option[ScopedRuntimeEnv] = None) extends RuntimeEnv {

  def updated(s: ESEnvComponent): ScopedRuntimeEnv =
    new ScopedRuntimeEnv(name, level, members.updated(s.name, s), parentOpt)

  def emptyChild(n: String): ScopedRuntimeEnv = ScopedRuntimeEnv.empty(n, level + 1)

  override def get(id: String): Option[ESEnvComponent] = super.get(id) match {
    case None => parentOpt.flatMap(_.get(id))
    case Some(r) => Some(r)
  }

  override def toString: String = s"<ScopedContext name=$name lvl=$level size=${members.size}>"
}

object ScopedRuntimeEnv {

  def apply(name: String,
            level: Int,
            members: Map[String, ESEnvComponent] = Map.empty,
            parentOpt: Option[ScopedRuntimeEnv] = None): ScopedRuntimeEnv =
    new ScopedRuntimeEnv(name, level, members, parentOpt)

  def empty(n: String, l: Int): ScopedRuntimeEnv =
    new ScopedRuntimeEnv(n, l, Map.empty, None)

  def initialized(n: String, l: Int, env: Map[String, ESEnvComponent]): ScopedRuntimeEnv =
    new ScopedRuntimeEnv(n, l, env, None)
}
