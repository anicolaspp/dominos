/**
  * Created by anicolaspp on 2/3/17.
  */

import java.net.InetSocketAddress

import Judge.Disconnected
import ParserError._
import PlayerHandler._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp
import akka.util.ByteString
import cats.Show
import cats.Show.ops._

class PlayerHandler(id: Int, address: InetSocketAddress, judge: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case Tcp.Received(data) => toCommand(data.utf8String.trim) match {

      case Right(Ready) =>  sender() ! Tcp.Write(ByteString(s"You are player number ${id}\n"))

      case Left(error)  =>  {
        log.debug(error.toString)
        sender() ! Tcp.Write(ByteString(error.show))
      }
    }

    case Tcp.PeerClosed =>  {
      judge ! Disconnected(id, address)
    }

    case x  => log.debug(x.toString)
  }
}

object PlayerHandler {

  def props(id: Int, address: InetSocketAddress, judge: ActorRef): Props = Props(new PlayerHandler(id, address, judge))

  def toCommand(input: String): Either[ParserError, PlayerCommand] = PlayerCommand.parseCommand(input)
}

sealed trait ParserError
case class InvalidInput(input: String) extends ParserError

object ParserError {

  implicit def toShowable: Show[ParserError] = Show.show[ParserError] {
    case InvalidInput(input) => s"Invalid Input: $input"
  }
}

sealed trait PlayerCommand

case object Ready extends PlayerCommand
object PlayerCommand {

  def parseCommand(txt: String): Either[ParserError, PlayerCommand] = txt match {
    case "ready"  =>  Right(Ready)
    case _        =>  Left(InvalidInput(txt))
  }

  implicit def toShowable: Show[PlayerCommand] = Show.show[PlayerCommand] {
    case Ready  =>  "ready"
  }

}