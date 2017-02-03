/**
  * Created by anicolaspp on 2/2/17.
  */

import java.net.InetSocketAddress

import Commander.{GameOver, StartGameSession}
import Judge.{Add, Start}
import akka.actor.{Actor, ActorLogging, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString

import scala.collection.mutable.ListBuffer


class Commander(endpoint: InetSocketAddress) extends Actor with ActorLogging{

  val judge = context.actorOf(Props[Judge])

  override def receive: Receive = {

    case StartGameSession         =>  IO(Tcp)(context.system) ! Tcp.Bind(self, endpoint)
    case Tcp.Bound(localAddress)  =>  {
      log.debug(s"judge running on $localAddress")

      context.become(waitingForPlayers(4, List.empty[InetSocketAddress]))
    }
  }

  def waitingForPlayers(numberOfPlayers: Int, players: List[InetSocketAddress]): Receive = {

    case Tcp.Connected(playerAddress, _) =>  if (numberOfPlayers == 0) {
      judge ! Start(players)

      context.become(waitingGameOver)
    } else {
      log.debug(s"New player has connected. We need $numberOfPlayers more player")

      sender() ! Tcp.Register(judge)

      judge ! Add(playerAddress)

      waitingForPlayers(numberOfPlayers - 1, players :+ playerAddress)
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


class Judge extends Actor with ActorLogging {

  val players = ListBuffer.empty[InetSocketAddress]

  override def receive: Receive = {

    case Add(player)  => players.append(player)

    case Tcp.Received(data) => data.utf8String.trim match {
      case "ready"  =>  sender() ! Tcp.Write(ByteString(s"You are player number ${players.length}"))
    }
  }
}

object Judge {

  case class Start(players: List[InetSocketAddress])
  case class Add(player: InetSocketAddress)
}