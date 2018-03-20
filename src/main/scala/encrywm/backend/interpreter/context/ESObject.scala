package encrywm.backend.interpreter.context

case class ESObject(name: String, attrs: Seq[ESValue]) extends ESCtxComponent

object ESObject {
  val typeId: Byte = 0.toByte
}
