package encrywm.cli.commands

import encrywm.cli.Response
import encrywm.common.SourceValidator

import scala.util.Try

object Validate extends Command {

  override def execute(args: Array[String]): Option[Response] = Try {
    SourceValidator.validateFromFile(args.head)
  }.map(_ => Some(Response(0))).getOrElse(Some(Response(200)))
}
