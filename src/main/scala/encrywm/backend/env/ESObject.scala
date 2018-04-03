package encrywm.backend.env

import encrywm.core.Types.{ESProduct, ESType}

case class ESObject(name: String,
                    attrs: Map[String, ESValue],
                    ancestorOpt: Option[ESObject] = None) extends ESEnvComponent {

  def getAttr(n: String): Option[ESValue] = attrs.get(n)
    .orElse(ancestorOpt.flatMap(_.getAttr(n)))

  def isInstanceOf(t: ESType): Boolean = t match {
    case p: ESProduct => this.name == p.ident && this.attrs.zip(p.fields).forall { case ((n1, v1), (n2, t2)) =>
      n1 == n2 && v1.tpe == t2
    }
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
