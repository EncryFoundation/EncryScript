package encrywm.backend.executor.context

trait RuntimeContext {

  val members: Map[String, ESRuntimeComponent]

  def updated(s: ESRuntimeComponent): RuntimeContext

  def get(id: String): Option[ESRuntimeComponent] = members.get(id)
}
