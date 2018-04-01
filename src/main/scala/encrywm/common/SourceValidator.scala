package encrywm.common

import scala.util.{Failure, Success}

object SourceValidator {

  case class ValidationFailure(reason: String)

  case object ValidationSuccess

  type ValidationResult = Either[ValidationFailure, ValidationSuccess.type]

  def validateSource(source: String): ValidationResult = {
    SourceProcessor.process(source) match {
      case Failure(e) =>
        Left(ValidationFailure(e.getMessage))
      case Success(_) =>
        Right(ValidationSuccess)
    }
  }

  def validateFromFile(path: String): ValidationResult = {
    if (!path.endsWith(".esc")) return Left(ValidationFailure("Wrong file extension."))
    val source = scala.io.Source.fromFile(path).mkString
    SourceProcessor.process(source) match {
      case Failure(e) =>
        Left(ValidationFailure(e.getMessage))
      case Success(_) =>
        Right(ValidationSuccess)
    }
  }
}
