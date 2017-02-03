/**
  * Created by anicolaspp on 2/2/17.
  */

import java.net.InetSocketAddress

import Commander.{GameOver, StartGameSession}
import Judge.{Add, GetReady, Start}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString

import scala.collection.mutable.ListBuffer


class Commander(endpoint: InetSocketAddress) extends Actor with ActorLogging{

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


class Judge(commander: ActorRef) extends Actor with ActorLogging {

  val players = ListBuffer.empty[ActorRef]

  override def receive: Receive = {
    case GetReady(endpoint) => IO(Tcp)(context.system) ! Tcp.Bind(self, endpoint)

    case Tcp.Bound(localAddress) => {
      log.debug(s"judge running on $localAddress")
      log.debug("waiting for players")
      
      context.become(waitingForPlayers(4, List.empty))
    }

  }

  def playing: Receive = {
    case Start      =>  log.debug("starting game")
  }

  def waitingForPlayers(numberOfPlayers: Int, players: List[InetSocketAddress]): Receive = {

    case Tcp.Connected(playerAddress, _) =>  {
      log.debug(s"New player has connected.")

      if (numberOfPlayers == 0) {
        context.become(playing)
        self ! Start

      } else {

        log.debug(s"We need ${numberOfPlayers - 1} more players.")

        self ! Add(playerAddress, sender())

        context.become(waitingForPlayers(numberOfPlayers - 1, players :+ playerAddress))
      }
    }

    case Add(address, conn)  => {
      val playerHandler = context.actorOf(PlayerHandler.props(players.length, address), name = players.length.toString)

      conn ! Tcp.Register(playerHandler)

      this.players.append(playerHandler)
    }


  }
}

object Judge {

  def props(commander: ActorRef): Props = Props(new Judge(commander))

  case object Start
  case class Add(player: InetSocketAddress, connection: ActorRef)
  case class GetReady(endpoint: InetSocketAddress)
}

class PlayerHandler(id: Int, address: InetSocketAddress) extends Actor with ActorLogging {
  override def receive: Receive = {
    case Tcp.Received(data) => data.utf8String.trim match {
      case "ready"  =>  sender() ! Tcp.Write(ByteString(s"You are player number ${id}"))
      case x        =>  {
        log.debug(x)
        sender() ! Tcp.Write(ByteString("invalid input"))
      }
    }

    case x  => log.debug(x.toString)
  }
}

object PlayerHandler {

  def props(id: Int, address: InetSocketAddress): Props = Props(new PlayerHandler(id, address))

}

