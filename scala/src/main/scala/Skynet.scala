import scala.concurrent.Future
import akka.actor.{ ActorSystem, Props, ActorRef, Actor }
import akka.pattern.pipe
import akka.routing. { ActorRefRoutee, RoundRobinRoutingLogic, Router }

object Skynet {
  val props = Props[Skynet]
  case class Start(level: Int, num: Long)
}

class Skynet extends Actor {
  import Skynet._

  var todo = 10
  var count = 0L

  var replyTo: ActorRef = null

  def receive = {
    case Start(level, num) =>
      replyTo = sender
      if (level == 1) {
        replyTo ! num
        // context.stop(self)
      } else {
        val start = num * 10
        (0 to 9) foreach (i => Root.router.route(Start(level - 1, start + i), sender))
      }
    case l: Long =>
      todo -= 1
      count += l
      if (todo == 0) {
        replyTo ! count
        // context.stop(self)
      }
  }
}

class Root extends Actor {
  import Root._

  implicit val ec = context.dispatcher

  def skynet1(level: Int, num: Long = 0): Future[Long] = {
    if (level == 1) {
      Future.successful(num)
    } else {
      val start = num * 10
      val futures = (0 to 9) map { i => skynet1(level - 1, start + i) }
      Future.sequence(futures).map(_.sum)
    }
  }

  def skynet2(level: Int, num: Long = 0): Long = {
    if (level == 1) {
      num
    } else {
      val start = num * 10
      val futures = (0 to 9) map { i => skynet2(level - 1, start + i) }
      futures.sum
    }
  }

  override def receive = {
    case Run(n) => startRun(n)
  }

  def startRun(n: Int): Unit = {
    System.gc()
    Thread.sleep(1000)
    val start = System.nanoTime()
    skynet1(8).pipeTo(self)
    context.become(waiting(n - 1, start))
  }

  def startRun2(n: Int): Unit = {
    System.gc()
    Thread.sleep(1000)
    val start = System.nanoTime()
    Future(skynet2(8)).pipeTo(self)
    context.become(waiting(n - 1, start))
  }

  def waiting(n: Int, start: Long): Receive = {
    case x: Long =>
      val diffMs = (System.nanoTime() - start) / 1000000
      println(s"Result: $x in $diffMs ms.")
      if (n == 0) context.system.terminate()
      else startRun(n)
  }
}

object Root extends App {
  case class Run(num: Int)

  val system = ActorSystem("main")

  val router = {
    val routees = Vector.fill(5) {
      val r = system.actorOf(Skynet.props)
      // context watch r
      ActorRefRoutee(r)
    }
    Router(RoundRobinRoutingLogic(), routees)
  }

  system.actorOf(Props[Root]) ! Run(8)
}
