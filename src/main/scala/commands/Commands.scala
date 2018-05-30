package commands

import Repository._
import poll.Poll
import com.github.nscala_time.time.Imports._
import scala.util.{Success, Try}

case class Commands(user: (Int, String, String)) {
  val userID: Int = user._1

  private def formatTime(time: String): Try[Option[DateTime]] = {
    if (time != null)
      Try(Some(DateTimeFormat.forPattern("hh:mm:ss yy:MM:dd").parseDateTime(time)))
    else Success(None)
  }

  def createPoll(title: String, anonymous: Boolean,
                 viewType: String,
                 startTime: String,
                 stopTime: String): String = {
    val id = AllPolls.get_id()
    val fStartTime = formatTime(startTime)
    val fStopTime = formatTime(stopTime)
    if (fStartTime.isSuccess && fStopTime.isSuccess) {
      AllPolls.set(id, Poll(id, title, anonymous, viewType, fStartTime.get, fStopTime.get))
      "Success: " + id
    }
    else "Your time is broken!"
  }

  def pollList: String = {
    if (AllPolls.getAll.nonEmpty)
      AllPolls.getAll.toVector.sortBy(e => e._1.toInt).map(e =>
        s"${e._2.id} => ${e._2.name}\nis anonymous? ${e._2.isAnonymous}\nis it running? ${e._2.isRun}" +
          s"\nis over? ${!e._2.isRun}\n${e._2.viewType}\nstarts in: ${e._2.startTime
            .map(_.toString("hh:mm:ss yy:MM:dd")).getOrElse("not set")}\n" +
          s"ends in: ${e._2.stopTime.map(_.toString("hh:mm:ss yy:MM:dd")).getOrElse("not set")}")
        .mkString("\n\n")
    else
      "Can You see the list of Your polls? I can't too. But they exists."
  }

  def deletePoll(id: Int): String = {
    if (AllPolls.getRun(id).isSuccess || AllPolls.get(id).isSuccess) {
      AllPolls.remove(id)
      "Exterminate! Exterminate! Exterminate!"
    } else "Can't delete Your Poll, cuz there's no such one!"
  }

  def startPoll(id: Int): String = {
    if (AllPolls.get(id).isSuccess && AllPolls.get(id).get.startTime.isEmpty) {
      AllPolls.setRun(id)
      "Your poll was just started, look for feedback!"
    } else if (AllPolls.get(id).isFailure)
      "Can't start your Poll, cuz there's no such one!"
    else "The Poll will start itself when the time come! Wait..."
  }

  def stopPoll(id: Int): String = {
    if (AllPolls.getRun(id).isSuccess && AllPolls.get(id).get.stopTime.isEmpty) {
      AllPolls.removeRun(id)
      "Your poll was just finished, that was a great poll!"
    } else if (AllPolls.getRun(id).isFailure)
      "Cant't stop Your Poll, cuz it's not run!"
    else "The Poll will stop itself when the time come! Wait..."
  }

  def pollResult(id: Int): String = {
    if (AllPolls.get(id).isSuccess) {
      AllPolls.get(id).map(p => {
        if ((p.viewType.eq("continuous") || p.isOver) && p.isAnonymous) {
          s"Poll №${p.id} ${p.name}:\n${
            p.questions.indices.map(q =>
              s"${q.toString}. ${p.questions(q)._1} (${p.questions(q)._2}):\n" +
                (if (p.questions(q)._2 == "open")
                  s"${
                    (for (i <- Inner.get(p.id, q, 0))
                      yield "\t" + i._2).mkString("\n")
                  }"
                else p.answers(q).indices.map(a =>
                  s"\t$a) ${p.answers(q)(a)} - ${Inner.get(p.id, q, a).length} votes").mkString("\n"))
            ).mkString("\n")
          }"
        }
        else if ((p.viewType.eq("continuous") || p.isOver) && !p.isAnonymous) {
          s"Poll №${p.id} ${p.name}:\n${
            p.questions.indices.map(q =>
              s"${q.toString}. ${p.questions(q)._1} (${p.questions(q)._2}):\n" +
                (if (p.questions(q)._2 == "open")
                  s"\t${
                    (for (i <- Inner.get(p.id, q, 0))
                      yield i._2 + " -> " + i._1._2 + " " + i._1._3).mkString("\n")
                  }"
                else p.answers(q).indices.map(a =>
                  s"\t$a) ${p.answers(q)(a)}: ${
                    (for (i <- Inner.get(p.id, q, a))
                      yield i._1._2 + i._1._3).mkString("\n")
                  }")
                  )
            ).mkString("\n")
          }"
        }
        else "You can view the result only when the poll will be over"
      }).get
    }
    else "We don't have such a Poll"
  }

  def addQuestion(question: String, qType: String, answers: Vector[String]): String = {
    if (CurrentPoll.get(userID).isSuccess)
      CurrentPoll.get(userID).map(cp => {
        val poll = Poll.set_question(question, qType, answers)(cp)
        if (!AllPolls.containsRun(cp)) {
          CurrentPoll.set(userID, poll)
          AllPolls.set(cp.id, poll)
          val id = CurrentPoll.get(userID).get.questions.indexOf((question, qType))
          "Success: " + id.toString
        } else "It's already run, You should bear with it!"
      }).get
    else "There's no such current poll"
  }

  def begin(id: Int): String = {
    if (AllPolls.get(id).isSuccess && CurrentPoll.get(userID).isFailure) {
      CurrentPoll.set(userID, AllPolls.get(id).get)
      "Let's Rock!"
    }
    else if (AllPolls.get(id).isSuccess && CurrentPoll.get(userID).isSuccess)
      "You've already begun one Poll!"
    else "There is no such Poll!"
  }

  def end: String = {
    if (CurrentPoll.get(userID).isSuccess && Try(CurrentPoll.setNone(userID)).isSuccess)
      "Now, you're free!"
    else "You have no begun Poll!"
  }

  def view: String = {
    if (CurrentPoll.get(userID).isSuccess)
      CurrentPoll.get(userID).map(cp =>
        s"Poll №${cp.id} ${cp.name}:\n${
          cp.questions.indices.map(q =>
            s"${q.toString}. ${cp.questions(q)._1} (${cp.questions(q)._2}):\n" + cp.answers(q).indices.map(a =>
              s"\t$a) ${cp.answers(q)(a)}").mkString("\n")).mkString("\n")
        }"
      ).get
    else "There's no current Poll!"
  }

  def deleteQuestion(number: Int): String = {
    if (Try(CurrentPoll.get(userID).get.questions(number)).isSuccess)
      CurrentPoll.get(userID).map(cp => {
        val poll = Poll.delete_question(cp.questions(number), number)(cp)
        if (!AllPolls.containsRun(cp)) {
          CurrentPoll.set(userID, poll)
          AllPolls.set(cp.id, poll)
          "Success"
        } else "it's runned, you should bear with it!"
      }).get
    else "Can't delete this Question, set the right Poll or question!"
  }

  def answer(number: Int, answer: String): String = {
    if (Try(CurrentPoll.get(userID).get.questions(number)).isSuccess) {
      val currentPoll = CurrentPoll.get(userID).get
      val answers = Try(answer.split(" ").map(_.toInt))
      val answered: Boolean = if (currentPoll.questions(number)._2 == "open")
        Inner.get(currentPoll.id, number, 0).exists(e => e._1 == user)
      else currentPoll.answers(number).indices.exists(a =>
        Inner.get(currentPoll.id, number, a).exists(e => e._1 == user)
      )
      if (answered)
        "You've already answered it!"
      else {
        currentPoll.questions(number)._2 match {
          case "open" => if (!answer.isEmpty) {
            Inner.set(currentPoll.id, number, 0, user, answer)
            "Success"
          } else "Type of the question - Open, your answer is incorrect"
          case "choice" => if (Try(currentPoll.answers(number)(answers.get.head)).isSuccess &&
            answers.get.length == 1) {
            Inner.set(currentPoll.id, number, answers.get.head, user, "")
            "Success"
          } else "Type of the question - Choice, your answer is incorrect"
          case "multi" => if (answers.isSuccess && !answers.get.isEmpty &&
            Try(answers.get.map(e => currentPoll.answers(number)(e))).isSuccess) {
            answers.get.foreach(a => Inner.set(currentPoll.id, number, a, user, ""))
            "Success"
          } else "Type of the question - Multi, your answer is incorrect"
        }
      }
    } else "There's no such question or you've not chosen the Poll"
  }
}