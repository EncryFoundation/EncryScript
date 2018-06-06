package encrywm.lang.frontend.semantics

import encrywm.ast.Ast.{EXPR, Identifier, STMT}
import encrywm.ast.Ast
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256


class Optimizer {

  private var replacements = Seq.empty[(String, String)]

  def optimize(node: Ast.AST_NODE): Ast.AST_NODE = rewrite(manytd(strategy[Ast.AST_NODE]({
    case STMT.Let(EXPR.Declaration(EXPR.Name(Identifier(n), t), to), v, g) if n.length > 3 =>
      val name = Base58.encode(Blake2b256.hash(n)).take(3)
      replacements = replacements :+ (n, name)
      Some(STMT.Let(EXPR.Declaration(EXPR.Name(name, t), to), v, g))
    case EXPR.Name(Identifier(n), t) if replacements.exists(_._1 == n) =>
      Some(EXPR.Name(Identifier(replacements.find(_._1 == n).get._2), t))
    case _ => None
  })))(node)
}
