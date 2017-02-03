/**
  * Created by anicolaspp on 2/3/17.
  */

import java.net.InetSocketAddress

import Judge.{Add, Disconnected, GetReady, Start}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}

import scala.collection.mutable.ListBuffer

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
      val playerHandler = context.actorOf(PlayerHandler.props(players.length, address, judge = self), name = players.length.toString)

      conn ! Tcp.Register(playerHandler)

      this.players.append(playerHandler)
    }

    case Disconnected(playerId, address)  =>  {
      log.debug(s"Player $playerId running at $address has disconnected...")
      //TODO remove player
    }
  }

  def playing: Receive = {
    case Start      =>  log.debug("starting game")
    case Disconnected(playerId, address)  =>  {
      log.debug(s"Player $playerId running at $address has disconnected...")
    }
  }
}

object Judge {

  def props(commander: ActorRef): Props = Props(new Judge(commander))

  case object Start
  case class Add(player: InetSocketAddress, connection: ActorRef)
  case class GetReady(endpoint: InetSocketAddress)
  case class Disconnected(playerId: Int, address: InetSocketAddress)
}