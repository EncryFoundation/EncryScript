package encrywm.lang.frontend.semantics

import encrytl.core.Schema
import encrywm.ast.Ast
import encrywm.ast.Ast.{EXPR, Identifier}
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{manytd, rewrite, strategy}

object SchemaBinder {

  def bind(node: Ast.AST_NODE, schemas: Seq[Schema]): Ast.AST_NODE = rewrite(manytd(strategy[Ast.AST_NODE]({
    case EXPR.SchemaMatching(n, Identifier(schemaId)) if schemas.exists(_.ident == schemaId) =>
      Some(EXPR.SchemaMatching(n, Identifier(schemaId)))
    case _ => None
  })))(node)
}
