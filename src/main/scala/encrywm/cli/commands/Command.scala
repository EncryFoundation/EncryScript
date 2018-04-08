package encrywm.cli.commands

import encrywm.cli.Response

trait Command {

  def execute(args: Array[String]): Option[Response]
}
