package encrywm.backend.env

import encrywm.lib.Types.{ESProduct, ESType}

case class ESObject(name: String,
                    attrs: Map[String, ESValue],
                    tpe: ESProduct) extends ESEnvComponent {

  def getAttr(n: String): Option[ESValue] = attrs.get(n)

  def isInstanceOf(t: ESType): Boolean = t match {
    case p: ESProduct => this.name == p.ident && (this.tpe == p || this.tpe.isSubtypeOf(p))
    case _ => false
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case ESObject(thatName, thatAttrs, _) =>
      thatName == this.name && thatAttrs.zip(this.attrs).forall { case ((_, v1), (_, v2)) => v1 == v2 }
    case _ => false
  }
}

object ESObject {
  val typeId: Byte = 0.toByte
}
