/**
  * Created by anicolaspp on 2/2/17.
  */

import java.net.InetSocketAddress

import Commander.{GameOver, StartGameSession}
import Judge.GetReady
import akka.actor.{Actor, ActorLogging, Props}


class Commander(endpoint: InetSocketAddress) extends Actor with ActorLogging {

  val judge = context.actorOf(Judge.props(self), name = "judge")

  override def receive: Receive = {
    case StartGameSession         => {
      judge ! GetReady(endpoint)
      context.become(waitingGameOver)
    }
  }

  def waitingGameOver: Receive = {
    case GameOver  => log.debug("game ended")
  }
}


object Commander {

  def props(port: Int): Props = Props(new Commander(new InetSocketAddress("localhost", port)))

  case object StartGameSession
  case object GameOver
}










