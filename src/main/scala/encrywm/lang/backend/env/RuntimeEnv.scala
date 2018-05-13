package encrywm.lang.backend.env

trait RuntimeEnv {

  val members: Map[String, ESEnvComponent]

  def updated(s: ESEnvComponent): RuntimeEnv

  def get(id: String): Option[ESEnvComponent] = members.get(id)
}
