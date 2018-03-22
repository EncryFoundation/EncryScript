package encrywm.backend.evaluator.context.predefindFunctions

import encrywm.ast.Ast.AST_NODE
import encrywm.backend.evaluator.EvaluationError
import encrywm.backend.evaluator.context.{ESPredFunc, ESValue}
import encrywm.builtins.Types
import encrywm.builtins.Types.{BOOLEAN, BYTE_VECTOR, TYPE}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.util.Try

case class CheckSig() extends ESPredFunc{

  override val name: String = "CheckSig"

  override val eval: Seq[ESValue] => Try[ESValue] = (args: Seq[ESValue]) => Try{

    if (args.length != 3) throw EvaluationError(s"CheckSig function require 3 args, current count of args is ${args.length}")

    args.foreach(arg => if(!arg.tpe.isInstanceOf[BYTE_VECTOR.type]) throw EvaluationError(s"Incorrect type of argument for checksig()"))

    val funcArgs = args.foldLeft(Seq[Array[Byte]]()){
      (seq, arg) => seq :+ arg.value.asInstanceOf[Array[Byte]]
    }

    ESValue("checkSig", BOOLEAN)(Curve25519.verify(
      Signature @@ funcArgs.head,
      funcArgs(1),
      PublicKey @@ funcArgs(2)
    ))
  }
}
