package encrywm.common

import encrywm.ast.Ast.TREE_ROOT.Contract
import encrywm.ast.AstCodec.codec
import scodec.bits.BitVector

import scala.util.Try

object ScriptSerializer {

  def serialize(contract: Contract): SerializedScript =
    codec.encode(contract).require.toByteArray

  def deserialize(bytes: SerializedScript): Try[Contract] =
    Try(codec.decode(BitVector(bytes)).require.value.asInstanceOf[Contract])
}
