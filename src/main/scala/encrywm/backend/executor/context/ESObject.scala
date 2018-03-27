package encrywm.backend.executor.context

case class ESObject(name: String, attrs: Map[String, ESValue]) extends ESRuntimeComponent {

  def getAttr(n: String): Option[ESValue] = attrs.get(n)
}

object ESObject {
  val typeId: Byte = 0.toByte
}
