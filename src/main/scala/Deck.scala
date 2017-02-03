import Token.{Token, Value}

/**
  * Created by anicolaspp on 2/3/17.
  */
object Deck {

  def tokens: List[Token] =
    (0 to 9).flatMap { up =>
      (0 to 9).map { down =>
        Token(Value(up), Value(down))
      }
    }.toList
}
