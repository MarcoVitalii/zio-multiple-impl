import zio.{ExitCode, Has, ULayer, URIO, ZIO, ZLayer}
import zio.clock.Clock
import zio.random.Random
import zio.console._
import zio.Ref

trait CheckInterface {
  def name: String
  def execute(params: String): ZIO[Any, Throwable, Boolean]
}

/*to add a new implementation:
  1. add the case class that extends the interface
  2. create a layer for it that registers the impl on the checkService
  3. add the layer to the final environment (both on the type of the env and into the inject)
 */
case class CheckExa(random: Random.Service, console: Console.Service) extends CheckInterface {
  def name = "CheckExa"

  override def execute(params: String): ZIO[Any, Throwable, Boolean] = console.putStrLn(s"I'm exa $params").as(false)
}

object CheckExa {
  val live =
    ZLayer.fromServicesM[Random.Service, CheckService.Service, Console.Service, Any, Throwable, CheckExa] {
      (random, check, console) =>
        for {
          impl <- ZIO.succeed(CheckExa(random, console))
          _    <- check.register(impl.name, impl)
        } yield impl
    }
}

case class CheckMat(clock: Clock.Service, console: Console.Service) extends CheckInterface {
  def name = "CheckMat"

  override def execute(params: String): ZIO[Any, Throwable, Boolean] = console.putStrLn(params).as(false)
}

object CheckMat {

  val live =
    ZLayer.fromServicesM[Clock.Service, CheckService.Service, Console.Service, Any, Throwable, CheckMat] {
      (clock, check, console) =>
        for {
          impl <- ZIO.succeed(CheckMat(clock, console))
          _    <- check.register(impl.name, impl)
        } yield impl
    }
}

object CheckService {
  type CheckService = Has[Service]
  trait Service {
    def register(name: String, impl: CheckInterface): ZIO[Any, Throwable, Unit]
    def toImpl(name: String): ZIO[Any, Throwable, CheckInterface]
  }

  final case class Impl(ref: Ref[Map[String, CheckInterface]], console: Console.Service) extends Service {

    def register(name: String, impl: CheckInterface) =
      console.putStrLn(s"adding $name") *> ref.update(_ + (name -> impl))

    def toImpl(name: String): ZIO[Any, Throwable, CheckInterface] = ref.get.map(_(name))

  }
  val live = ZLayer.fromServiceM[Console.Service, Any, Throwable, Service] { console =>
    Ref.make(Map.empty[String, CheckInterface]).map(ref => Impl(ref, console))
  }
  def toImpl(name: String): ZIO[CheckService, Throwable, CheckInterface] = ZIO.serviceWith(_.toImpl(name))
}
case class EventToHandle(how: String, params: String)

object main extends zio.App {
  import zio.magic._
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    type requiredEnv = Has[CheckService.Service] with Has[CheckExa] with Has[CheckMat] with Console

    val toProcess = List(EventToHandle("CheckMat", "doing mat"), EventToHandle("CheckExa", ", hello exa!"))
    val prog: ZIO[requiredEnv, Throwable, Unit] = ZIO.foreach_(toProcess) { ev =>
      for {
        _    <- putStrLn(s"processing $ev")
        impl <- CheckService.toImpl(ev.how)
        _    <- impl.execute(ev.params)
        _    <- putStrLn(s"done")
      } yield ()
    }
    prog.injectCustom(CheckService.live, CheckMat.live, CheckExa.live).exitCode
  }

}
