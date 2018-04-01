package encrywm.common

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.AstCodec._
import encrywm.frontend.parser.Statements
import encrywm.frontend.semantics.{ComplexityAnalyzer, Optimizer, StaticAnalyser, Transformer}
import fastparse.all._

import scala.util.Try

object SourceProcessor {

  type SerializedContract = Array[Byte]

  def process(s: String): Try[Contract] = Try {
    val parsed = (Statements.contract ~ End).parse(s).get.value
    StaticAnalyser.scan(parsed)
    val transformed = Transformer.scan(parsed)
    val optimized = Optimizer.scan(transformed)
    optimized.asInstanceOf[Contract]
  }

  def source2Contract(s: String): Try[ESContract] = process(s).map { c =>
    val complexityScore = ComplexityAnalyzer.scan(c)
    val serializedScript = ScriptSerializer.serialize(c)
    ESContract(serializedScript, ScriptMeta(complexityScore))
  }

  def source2SerializedContract(s: String): Try[SerializedContract] = process(s).map { p =>
    codec.encode(p).require.toByteArray
  }
}
