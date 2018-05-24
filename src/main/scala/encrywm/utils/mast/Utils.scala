package encrywm.utils.mast

import encrywm.ast.Ast.Hash
import scorex.crypto.hash.Blake2b256

object Utils {

  def listRootHash(astNodeList: List[Array[Byte]]): Array[Byte] = {
    val hashList: List[Array[Byte]] = astNodeList.grouped(2).map(elemTuple =>
      if(elemTuple.length == 2) Blake2b256.hash(elemTuple.head ++ elemTuple.last)
      else elemTuple.head
    ).toList
    if(hashList.length != 1) listRootHash(hashList) else hashList.head
  }
}
