package encrywm.cli

import encrywm.cli.commands.{Validate, Command}
import jline.console.ConsoleReader

import scala.collection.mutable

object ConsolePromptListener extends App {

  override def main(args: Array[String]): Unit = {

    val prompt = "$> "

    val commands: mutable.HashMap[String,mutable.HashMap[String, Command]] = mutable.HashMap.empty

    commands.update("encvm", mutable.HashMap(
      "-validate" -> Validate
    ))

    val reader = new ConsoleReader()
    while (true) {
      val input = reader.readLine(prompt)
      if (input == "quit") return
      commands.get(parseCommand(input).head) match {
        case Some(value) =>
          parseCommand(input).slice(1, parseCommand(input).length).foreach { command =>
            value.get(command.split("=").head) match {
              case Some(cmd) =>
                println(cmd.execute(command.split("=")).map(_.code).getOrElse(200))
              case None =>
                println("Unsupported command. Type 'app -help' to get commands list")
            }
          }
        case None =>
          println("Unsupported command. Type 'app -help' to get commands list")
      }
    }
  }

  private def parseCommand(command: String): Seq[String] = {
    val commandsSeq = command.split(" ").toSeq
    commandsSeq
  }
}
