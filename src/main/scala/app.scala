import akka.actor.ActorSystem

/**
  * Created by anicolaspp on 2/2/17.
  */


object app {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("echo-server")

    val commander = system.actorOf(Commander.props(args(0).toInt))
    commander ! Commander.StartGameSession

    scala.io.StdIn.readLine()

    system.terminate()
  }
}
