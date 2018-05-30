package Repository

import poll.Poll

import scala.util.{Failure, Success, Try}

object AllPolls {
  private var P = Map[Int, Poll]()

  def get(id: Int): Try[Poll] =
    P.get(id).map(Success(_)).getOrElse(Failure(new Exception))

  def get(poll: Poll): Try[Poll] =
    Try(P.values.filter(p => p == poll).head)

  def set(id: Int, poll: Poll): Unit = P = P updated (id, poll)

  def remove(id: Int): Try[String] = Try(P -= id) match {
    case Success(s) => Success("Exterminate! Exterminate! Exterminate!")
    case Failure(f) => Failure(f)
  }

  def getAll: Map[Int, Poll] = P

  def get_id(): Int = {
    if (P.toList.nonEmpty) P.toList.maxBy(_._1)._1.toInt + 1 else 0
  }

  def getRun(id: Int): Try[Poll] =
    P.get(id).filter(_.isRun).map(Success(_)).getOrElse(Failure(new Exception()))

  def contains(poll : Poll) : Boolean = {
    P.values.toList.contains(poll)
  }

  def containsRun(poll : Poll) : Boolean = {
    getAllRun.values.toList.contains(poll)
  }

  def setRun(id: Int): Option[Unit] =
    P.get(id).filter(!_.isRun).map(p => P = P updated(id, p.copy(isRun = true)))

  def removeRun(id: Int): Option[Unit] =
    P.get(id).filter(_.isRun).map(p => P = P updated(id, p.copy(isRun = false, isOver = true)))

  def getAllRun: Map[Int, Poll] = P.filter(e => e._2.isRun)
}
