package encrywm.lang.frontend.semantics

import encrywm.ast.Ast.EXPR
import encrywm.ast.Ast
import encrywm.lib.Types.{ESFunc, ESList, ESOption}
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

object Transformer {

  def transform(node: Ast.AST_NODE): Ast.AST_NODE = rewrite(manytd(strategy[Ast.AST_NODE]({

    // Rule: Call(Attribute(coll, "exists"), Func) -> Exists(coll, Func)
    case EXPR.Call(EXPR.Attribute(value, attr, _), args, _, _)
      if value.tipe.isCollection &&
        attr.name == "exists" &&
        args.size == 1 &&
        args.head.tipe.isFunc =>
      Some(EXPR.Exists(value, args.head))

    // Rule: Call(Attribute(coll, "map"), Func) -> Map(coll, Func)
    case EXPR.Call(EXPR.Attribute(value, attr, _), args, _, tpe)
      if value.tipe.isCollection &&
        attr.name == "map" &&
        args.size == 1 &&
        args.head.tipe.isFunc =>
      Some(EXPR.Map(value, args.head, tpe))

    // Rule: Attribute(coll, "size") -> SizeOf(coll)
    case EXPR.Attribute(value, attr, _)
      if value.tipe.isCollection && attr.name == "size" =>
        Some(EXPR.SizeOf(value))

    // Rule: Attribute(coll, "sum") -> SizeOf(coll)
    case EXPR.Attribute(value, attr, _)
      if value.tipe.isCollection && attr.name == "sum" =>
      value.tipe match {
        case ESList(valT) =>
          Some(EXPR.Sum(value, valT))
        case _ => None
      }

    // Rule: Attribute(option, "isDefined") -> IsDefined(option)
    case EXPR.Attribute(value, attr, _)
      if value.tipe.isOption && attr.name == "isDefined" =>
      Some(EXPR.IsDefined(value))

    // Rule: Attribute(option, "get") -> Get(option)
    case EXPR.Attribute(value, attr, _) if attr.name == "get" =>
      value.tipe match {
        case ESOption(inT) => Some(EXPR.Get(value, inT))
        case ESFunc(_, ESOption(inT)) => Some(EXPR.Get(value, inT))
        case _ => None
      }

    case _ => None
  })))(node)
}