package frontend.semantics

import encrywm.frontend.parser.{Ast, Statements}
import encrywm.frontend.semantics.SemanticAnalyzer
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class SemanticAnalyzerSpec extends PropSpec with Matchers {

  property("Semantically correct simple AST analysis") {

    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |let a: int = 9
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe true
  }

  property("Semantically incorrect simple AST analysis (Undefined name)") {

    val simpleTreeParsed = (Statements.contract ~ End).parse(
      """
        |let a: undef = 9
      """.stripMargin)

    val analyzer = new SemanticAnalyzer

    simpleTreeParsed.isInstanceOf[Parsed.Success[Ast.STMT]] shouldBe true

    val analyzeTry = Try(analyzer.visit(simpleTreeParsed.get.value))

    analyzeTry.isSuccess shouldBe false
  }
}
