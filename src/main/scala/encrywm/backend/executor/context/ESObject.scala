package encrywm.backend.executor.context

case class ESObject(name: String, attrs: Map[String, ESValue]) extends ESCtxComponent

object ESObject {
  val typeId: Byte = 0.toByte
}
