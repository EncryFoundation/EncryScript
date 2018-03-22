package encrywm.backend.evaluator.context

import scala.collection.mutable

case class ESObject(name: String, attrs: mutable.TreeMap[String, ESValue]) extends ESCtxComponent

object ESObject {
  val typeId: Byte = 0.toByte
}
