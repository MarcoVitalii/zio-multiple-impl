import zio.{ExitCode, Has, ULayer, URIO, ZIO, ZLayer}
import zio.clock.Clock
import zio.random.Random
import zio.console._

object CheckService {
  trait Service {
    def name: String
    def execute(params: String): ZIO[Any, Throwable, Boolean]
  }
  type CheckMap = Map[String, Service]

  //we could provide an empty map that has to be updated while before building the final env with zio magic.
  lazy val liveAllImpl = ZLayer
    .succeed(Map.empty[String, Service])
    .flatMap(CheckMat.addImpl)
    .flatMap(CheckExa.addImpl)

  def get(name: String) =
    ZIO
      .service[CheckService.CheckMap]
      .flatMap(_.get(name) match {
        case None       => ZIO.fail(new RuntimeException(s"Missing implementation for $name"))
        case Some(impl) => ZIO.succeed(impl)
      })
}

case class CheckExa(random: Random.Service, console: Console.Service) extends CheckService.Service {
  def name = "CheckExa"

  override def execute(params: String): ZIO[Any, Throwable, Boolean] = console.putStrLn(s"I'm exa $params").as(false)
}

object CheckExa {
  def addImpl(hasMap: Has[CheckService.CheckMap]) =
    ZLayer.fromServices[Random.Service, Console.Service, CheckService.CheckMap] { (random, console) =>
      val impl = CheckExa(random, console)
      println("adding exa")
      hasMap.get + (impl.name -> impl)
    }

}

case class CheckMat(clock: Clock.Service, console: Console.Service) extends CheckService.Service {
  def name = "CheckMat"

  override def execute(params: String): ZIO[Any, Throwable, Boolean] = console.putStrLn(params).as(false)
}

object CheckMat {
  def addImpl(hasMap: Has[CheckService.CheckMap]) =
    ZLayer.fromServices[Clock.Service, Console.Service, CheckService.CheckMap] { (clock, console) =>
      val impl = CheckMat(clock, console)
      println("adding mat")
      hasMap.get + (impl.name -> impl)
    }

}

case class EventToHandle(how: String, params: String)

object main extends zio.App {
  import zio.magic._
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    type requiredEnv = Has[CheckService.CheckMap] with Console

    val toProcess = List(EventToHandle("CheckMat", "doing mat"), EventToHandle("CheckExa", ", hello exa!"))
    val prog: ZIO[requiredEnv, Throwable, Unit] = ZIO.foreach_(toProcess) { ev =>
      for {
        impl <- CheckService.get(ev.how)
        _    <- putStrLn(s"processing $ev")
        _    <- impl.execute(ev.params)
        _    <- putStrLn(s"done")
      } yield ()
    }
    prog.injectCustom(CheckService.liveAllImpl).exitCode

  }
}
