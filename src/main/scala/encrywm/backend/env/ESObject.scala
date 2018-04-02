package encrywm.backend.env

case class ESObject(name: String,
                    attrs: Map[String, ESValue],
                    ancestorOpt: Option[ESObject] = None) extends ESEnvComponent {

  def getAttr(n: String): Option[ESValue] = attrs.get(n)
    .orElse(ancestorOpt.flatMap(_.getAttr(n)))

  override def equals(obj: scala.Any): Boolean = obj match {
    case ESObject(thatName, thatAttrs, _) =>
      thatName == this.name && thatAttrs.zip(this.attrs).forall { case ((_, v1), (_, v2)) => v1 == v2 }
    case _ => false
  }
}

object ESObject {
  val typeId: Byte = 0.toByte
}
