package encrywm.app

import java.nio.file.FileSystems

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.alpakka.file.DirectoryChange
import akka.stream.alpakka.file.scaladsl.DirectoryChangesSource
import akka.stream.scaladsl.Source
import encrywm.app.Extensions._
import encrywm.common.SourceValidator

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.util.Try

object CompilerApp extends App {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher
  val fs = FileSystems.getDefault

  args.lift(0)
    .map(fs.getPath(_))
    .fold(println("Enter filePath as argument to main function.")) { filePath =>
      DirectoryChangesSource(filePath.getParent, pollInterval = 1.second, maxBufferSize = 10)
        .collect {
          case (path, DirectoryChange.Modification) if path == filePath => path
        }
        .prepend(Source.single(filePath))
        .map(path => scala.io.Source.fromFile(path.toString).mkString)
          .zipWithIndex
        .runForeach{ case( script, index) =>
          s"\nCompilation(${index+1})...".trace
          Try(SourceValidator.validateSource(script))
            .fold(_.getMessage.traceWith("Compile exception:\n" + _),
              _.trace("Compilation result:").trace
            )
        }
        .onComplete(_ => actorSystem.terminate())
    }
}
