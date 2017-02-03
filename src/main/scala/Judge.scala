/**
  * Created by anicolaspp on 2/3/17.
  */

import java.net.InetSocketAddress

import IdGenerator.{Id, NextId, Remove}
import Judge._
import Token.Token
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.util.Random

class Judge(commander: ActorRef) extends Actor with ActorLogging {

  implicit val timeout = Timeout(20 seconds)
  implicit val exceutionContext = context.dispatcher

  val players = ListBuffer.empty[(Int, ActorRef)]

  val idGenerator = context.actorOf(Props[IdGenerator])

  override def receive: Receive = {
    case GetReady(endpoint) => IO(Tcp)(context.system) ! Tcp.Bind(self, endpoint)

    case Tcp.Bound(localAddress) => {
      log.debug(s"judge running on $localAddress")
      log.debug("waiting for players")

      context.become(waitingForPlayers(4, List.empty))
    }

  }

  def waitingForPlayers(numberOfPlayers: Int, players: List[InetSocketAddress]): Receive = {

    case Tcp.Connected(playerAddress, _) => {
      log.debug(s"New player has connected.")
      log.debug(s"We need ${numberOfPlayers - 1} more players.")

      self ! Add(playerAddress, sender())
    }

    case Add(address, conn)  => {
      (idGenerator ? NextId).map {
        case Id(value)  => {
          val playerHandler = context.actorOf(PlayerHandler.props(value, address, judge = self), name = value.toString)

          conn ! Tcp.Register(playerHandler)

          this.players.append((value, playerHandler))
          this.players.foreach(p => println(p._1))


          if (numberOfPlayers == 1) {
            context.become(playing)
            self ! Start
          } else {
            context.become(waitingForPlayers(numberOfPlayers - 1, players :+ address))
          }
        }
      }
    }

    case Disconnected(playerId, address)  =>  {
      log.debug(s"Player $playerId running at $address has disconnected...")

      this.players.zipWithIndex.find { case (p, _) => p._1 == playerId }.map { case (p, index) => {
        log.debug(s"removing ${p._1}")
        this.players.remove(index)
      }}

      this.players.foreach(p => println(p._1))

      idGenerator ! Remove(playerId)
      context.become(waitingForPlayers(numberOfPlayers + 1, players))
    }
  }

  def playing: Receive = {
    case Start      =>  {
      log.debug("starting game")

      val selection = new TokenSelector(Deck.tokens).select
      println(selection._1)
      println(selection._2)
      println(selection._3)
      println(selection._4)
    }
    case Disconnected(playerId, address)  =>  {
      log.debug(s"Player $playerId running at $address has disconnected...")
    }
    case IncompleteGame =>  log.debug("incomplete game")
  }
}

object Judge {

  def props(commander: ActorRef): Props = Props(new Judge(commander))

  case object Start
  case class Add(player: InetSocketAddress, connection: ActorRef)
  case class GetReady(endpoint: InetSocketAddress)
  case class Disconnected(playerId: Int, address: InetSocketAddress)

  case object IncompleteGame
}



class TokenSelector(val tokens: List[Token]) {
  def select: (List[Token], List[Token], List[Token], List[Token]) = {

    def selectTen(selected: Int, xs: List[Token], left: List[Token]): (List[Token], List[Token]) = {
      if (selected == 10) {
        (xs, left)
      } else {

        val index = Random.nextInt(left.length)
        val one = left(index)

        selectTen(selected + 1, one :: xs, left.filterNot(_ == one))
      }
    }

    val (p1, l1) = selectTen(0, List.empty, tokens)
    val (p2, l2) = selectTen(0, List.empty, l1)
    val (p3, l3) = selectTen(0, List.empty, l2)
    val (p4, l4) = selectTen(0, List.empty, l3)

    assert(p1.toSet.union(p2.toSet.union(p3.toSet.union(p4.toSet))).toList.length == 40)

    (p1, p2, p3, p4)
  }
}





