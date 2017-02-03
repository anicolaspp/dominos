/**
  * Created by anicolaspp on 2/3/17.
  */

import cats.Show

sealed trait ParserError
case class InvalidInput(input: String) extends ParserError

object ParserError {

  implicit def toShowable: Show[ParserError] = Show.show[ParserError] {
    case InvalidInput(input) => s"Invalid Input: $input"
  }
}