/**
  * Created by anicolaspp on 2/3/17.
  */

import java.net.InetSocketAddress

import Judge.{Disconnected, IncompleteGame}
import ParserError._
import PlayerHandler._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp
import akka.util.ByteString
import cats.Show.ops._

class PlayerHandler(id: Int, address: InetSocketAddress, judge: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case Tcp.Received(data) => toCommand(data.utf8String.trim) match {
      case Right(Ready) =>  sender() ! Tcp.Write(ByteString(s"You are player number ${id}\n"))
      case Left(error)  =>  {
        log.debug(error.show)
        sender() ! Tcp.Write(ByteString(error.show))
      }
    }
    case Tcp.PeerClosed => judge ! Disconnected(id, address)
    case Tcp.ErrorClosed(cause) => {
      log.debug(cause)

      judge ! IncompleteGame
    }
  }
}

object PlayerHandler {

  def props(id: Int, address: InetSocketAddress, judge: ActorRef): Props = Props(new PlayerHandler(id, address, judge))

  def toCommand(input: String): Either[ParserError, PlayerCommand] = PlayerCommand.parseCommand(input)
}