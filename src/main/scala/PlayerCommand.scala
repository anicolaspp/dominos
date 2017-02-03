/**
  * Created by anicolaspp on 2/3/17.
  */

import cats.Show

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