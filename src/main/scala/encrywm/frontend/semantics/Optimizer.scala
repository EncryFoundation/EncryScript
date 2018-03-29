package encrywm.frontend.semantics

import encrywm.ast.Ast.{EXPR, Identifier}
import encrywm.ast.{Ast, AstNodeScanner}
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

object Optimizer extends AstNodeScanner {

  override def scan(node: Ast.AST_NODE): Ast.AST_NODE = rewrite(manytd(strategy[Ast.AST_NODE]({
    case EXPR.Name(Identifier(n), ctx, t) if n.length > 3 =>
      val name = n.take(1) + Base58.encode(Blake2b256.hash(n)).takeRight(2) // TODO: Short name generator.
      Some(EXPR.Name(name, ctx, t))
    case _ => None
  })))(node)
}
