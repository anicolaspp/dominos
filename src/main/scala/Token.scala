/**
  * Created by anicolaspp on 2/3/17.
  */
object Token {

  def apply(up: Value, down: Value): Token = new Token(up, down)

//  def fromValues(up: Int, down: Int): Either[TokenValue, Token] = (getValue(up), getValue(down)) match {
//    case (InvalidValue, _)  =>
//  }

  def getValue(value: Int): TokenValue = ???

  case class Token(up: Value, down: Value)
  sealed trait TokenValue
  case class Value(value: Int) extends TokenValue
  case object InvalidValue extends TokenValue
}
