/**
  * Created by anicolaspp on 2/3/17.
  */

import IdGenerator.{EmptyId, Id, NextId, Remove}
import akka.actor.{Actor, ActorLogging}

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
    }

    case Remove(id) =>  ids.find { case (i, _) => i == id } foreach { value => ids(value._1) = true }
  }
}

object IdGenerator {

  case object NextId
  case class Id(value: Int)
  case object EmptyId
  case class Remove(id: Int)
}