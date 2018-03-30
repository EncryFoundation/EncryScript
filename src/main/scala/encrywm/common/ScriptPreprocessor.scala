package encrywm.common

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.AstCodec._
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.{Optimizer, StaticAnalyser, Transformer}
import fastparse.all._

import scala.util.Try

object ScriptPreprocessor {

  type SerializedContract = Array[Byte]

  def process(s: String): Try[Contract] = Try {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    StaticAnalyser.scan(parsed)
    val transformed = Transformer.scan(parsed)
    val optimized = Optimizer.scan(transformed)
    optimized.asInstanceOf[Contract]
  }

  def processAndSerialize(s: String): Try[SerializedContract] = process(s).map { p =>
    codec.encode(p).require.toByteArray
  }
}
