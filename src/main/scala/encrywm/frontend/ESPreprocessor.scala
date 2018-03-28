package encrywm.frontend

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.StaticAnalyser
import encrywm.ast.AstCodec._
import fastparse.all._

import scala.util.Try

object ESPreprocessor {

  type SerializedContract = Array[Byte]

  def process(s: String): Try[Contract] = Try {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    StaticAnalyser.scan(parsed)
    parsed
  }

  def processAndSerialize(s: String): Try[SerializedContract] = process(s).map { p =>
    codec.encode(p).require.toByteArray
  }
}
