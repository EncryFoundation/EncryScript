package encrywm.lang.backend.env

import encrywm.lib.predef.PredefFunctions

case class ScopedRuntimeEnv(name: String,
                            level: Int,
                            override val members: Map[String, ESEnvComponent],
                            parentOpt: Option[ScopedRuntimeEnv] = None,
                            isFunc: Boolean = false) extends RuntimeEnv {

  def updated(s: ESEnvComponent): ScopedRuntimeEnv =
    ScopedRuntimeEnv(name, level, members.updated(s.id, s), parentOpt)

  def emptyChild(n: String): ScopedRuntimeEnv = ScopedRuntimeEnv.empty(n, level + 1, Some(this))

  def child(n: String,
            ms: Map[String, ESEnvComponent],
            isFunc: Boolean = false): ScopedRuntimeEnv = ScopedRuntimeEnv(n, level + 1, ms, Some(this), isFunc)

  override def get(id: String): Option[ESEnvComponent] = members.get(id).orElse(parentOpt.flatMap(_.get(id)))

  override def toString: String = s"<ScopedContext name=$name lvl=$level size=${members.size}>"
}

object ScopedRuntimeEnv {

  def empty(n: String, l: Int, parent: Option[ScopedRuntimeEnv] = None): ScopedRuntimeEnv =
    ScopedRuntimeEnv(n, l, Map.empty, parent)

  def initialized(n: String, l: Int, env: Map[String, ESEnvComponent]): ScopedRuntimeEnv =
    ScopedRuntimeEnv(n, l, env ++ PredefFunctions.all, None)
}
