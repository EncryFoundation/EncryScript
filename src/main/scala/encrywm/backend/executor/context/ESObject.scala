package encrywm.backend.executor.context

case class ESObject(name: String, attrs: Map[String, ESValue]) extends ESRuntimeComponent

object ESObject {
  val typeId: Byte = 0.toByte
}
