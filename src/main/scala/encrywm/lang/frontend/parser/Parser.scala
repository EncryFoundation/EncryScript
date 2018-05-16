package encrywm.lang.frontend.parser

import encrywm.ast.Ast
import fastparse.all._

import scala.util.{Failure, Success, Try}

object Parser {

  def parse(source: String): Try[Ast.TREE_ROOT.Contract] = ( Statements.contract ~ End ).parse(source) match {
    case r: Parsed.Success[Ast.TREE_ROOT.Contract] => Success(r.value)
    case e: Parsed.Failure => Failure(new Exception(e.msg))
  }
}
