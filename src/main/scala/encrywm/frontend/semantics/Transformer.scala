package encrywm.frontend.semantics

import encrywm.ast.Ast.EXPR
import encrywm.ast.{Ast, AstNodeScanner}
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

object Transformer extends AstNodeScanner {

  override def scan(node: Ast.AST_NODE): Ast.AST_NODE = rewrite(manytd(strategy[Ast.AST_NODE]({
    // Rule: Attribute(coll, "size") -> SizeOf(coll)
    case EXPR.Attribute(value, attr, _, _)
      if value.tpeOpt.get.isCollection && attr.name == "size" =>
        Some(EXPR.SizeOf(value))

    // Rule: Attribute(option, "isDefined") -> IsDefined(option)
    case EXPR.Attribute(value, attr, _, _)
      if value.tpeOpt.get.isOption && attr.name == "isDefined" =>
      Some(EXPR.IsDefined(value))

    // Rule: Attribute(option, "get") -> Get(option)
    case EXPR.Attribute(value, attr, _, _)
      if value.tpeOpt.get.isOption && attr.name == "get" =>
      Some(EXPR.Get(value))

    case _ => None
  })))(node)
}