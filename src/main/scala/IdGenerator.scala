import IdGenerator.{Id, Remove}
import akka.actor.{Actor, ActorLogging}

/**
  * Created by anicolaspp on 2/3/17.
  */
class IdGenerator extends Actor with ActorLogging {

  val ids = scala.collection.mutable.Map[Int, Boolean](
    1 -> true,
    2 -> true,
    3 -> true,
    4 -> true
  )

  override def receive: Receive = {
    case NextId =>  {
      ids.find { case (_, available) => available } match {
        case Some(value) => {
          sender() ! Id(value._1)
          ids(value._1) = false
        }
        case None =>  sender() ! EmptyId
      }

      ids.foreach {case (key, value) => log.debug(s"$key: $value")}
    }

    case Remove(id) =>  {
      ids.find { case (i, _) => i == id } match {
        case Some(value) => ids(value._1) = true
      }

      ids.foreach {case (key, value) => log.debug(s"$key: $value")}
    }
  }
}

object IdGenerator {

  case object NextId
  case class Id(value: Int)
  case object EmptyId
  case class Remove(id: Int)
}