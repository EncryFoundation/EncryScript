package encrywm.frontend

import scala.util.{Failure, Success}

object ESValidator {

  case class ValidationFailure(reason: String)

  case object ValidationSuccess

  type ValidationResult = Either[ValidationFailure, ValidationSuccess.type]

  def validateSource(source: String): ValidationResult = {
    ESPreprocessor.process(source) match {
      case Failure(e) =>
        Left(ValidationFailure(e.getMessage))
      case Success(_) =>
        Right(ValidationSuccess)
    }
  }

  def validateFromFile(path: String): ValidationResult = {
    if (!path.endsWith(".esc")) return Left(ValidationFailure("Wrong file extension."))
    val source = scala.io.Source.fromFile(path).mkString
    ESPreprocessor.process(source) match {
      case Failure(e) =>
        Left(ValidationFailure(e.getMessage))
      case Success(_) =>
        Right(ValidationSuccess)
    }
  }
}
