package encrywm.frontend.semantics

import encrywm.ast.Ast.EXPR
import encrywm.ast.{Ast, AstNodeScanner}
import encrywm.lib.Types.ESOption
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

object Transformer extends AstNodeScanner {

  override def scan(node: Ast.AST_NODE): Ast.AST_NODE = rewrite(manytd(strategy[Ast.AST_NODE]({

    // Rule: Call(Attribute(coll, "exists"), Lambda) -> Exists(coll, lambda)
    case EXPR.Call(EXPR.Attribute(value, attr, _, _), args, _, _)
      if value.tpeOpt.get.isCollection &&
        attr.name == "exists" &&
        args.size == 1 &&
        args.head.tpeOpt.get.isFunc =>
      Some(EXPR.Exists(value, args.head))

    // Rule: Attribute(coll, "size") -> SizeOf(coll)
    case EXPR.Attribute(value, attr, _, _)
      if value.tpeOpt.get.isCollection && attr.name == "size" =>
        Some(EXPR.SizeOf(value))

    // Rule: Attribute(option, "isDefined") -> IsDefined(option)
    case EXPR.Attribute(value, attr, _, _)
      if value.tpeOpt.get.isOption && attr.name == "isDefined" =>
      Some(EXPR.IsDefined(value))

    // Rule: Attribute(option, "get") -> Get(option)
    case EXPR.Attribute(value, attr, _, _) if attr.name == "get" =>
      value.tpeOpt.get match {
        case ESOption(inT) => Some(EXPR.Get(value, Some(inT)))
        case _ => None
      }

    case _ => None
  })))(node)
}