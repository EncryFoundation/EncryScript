package encrywm.frontend

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.{Binder, Optimizer, StaticAnalyser}
import encrywm.ast.AstCodec._
import fastparse.all._

import scala.util.Try

object ESPreprocessor {

  type SerializedContract = Array[Byte]

  def process(s: String): Try[Contract] = Try {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    StaticAnalyser.scan(parsed)
    val bound = Binder.scan(parsed)
    val optimized = Optimizer.scan(bound)
    optimized.asInstanceOf[Contract]
  }

  def processAndSerialize(s: String): Try[SerializedContract] = process(s).map { p =>
    codec.encode(p).require.toByteArray
  }
}
