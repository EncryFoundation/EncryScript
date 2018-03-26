package encrywm.builtins.environment.context

import encrywm.backend.executor.context.{ESObject, ESValue}
import encrywm.builtins.Types._
import encrywm.frontend.semantics.scope.{BuiltInTypeSymbol, VariableSymbol}

object ESTransaction extends ESContextComponentHolder {

  override val name: String = "transaction"

  def buildInstance(accountPubKey: Array[Byte],
                    timestamp: Long,
                    body: Array[Byte]): ESObject = {
    val attrs = Map(
      "accountPubKey" -> ESValue("accountPubKey", BYTE_VECTOR)(accountPubKey),
      "timestamp" -> ESValue("timestamp", LONG)(timestamp),
      "body" -> ESValue("body", BYTE_VECTOR)(body),
    )
    ESObject(name, attrs)
  }

  override lazy val symbol: BuiltInTypeSymbol = BuiltInTypeSymbol(name, attributes = attrSymbols)

  override val attrs: IndexedSeq[(String, TYPE)] = IndexedSeq(
    "accountPubKey" -> BYTE_VECTOR,
    "timestamp" -> LONG,
    "body" -> BYTE_VECTOR
  )

  // TODO: Complete for ComplexTypes
  private val attrSymbols = attrs.map { case (n, t) =>
      val typeS = BuiltInTypeSymbol(t.identifier)
      VariableSymbol(n, Some(typeS))
  }
}
