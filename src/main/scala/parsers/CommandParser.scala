package parsers

import commands.Commands

import scala.collection.immutable.HashMap
import scala.util.{Success, Try}


object CommandParser {
  val cmd : HashMap[String, ((Int, String, String), String) => Try[String]] =
    HashMap[String, ((Int, String, String), String) => Try[String]](
    "/create_poll" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.create_poll, msg).get).map(p => Commands(user).createPoll(p._1, p._2, p._3, p._4, p._5))),
    "/delete_poll" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.id_number, msg).get).map(p => Commands(user).deletePoll(p))),
    "/start_poll" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.id_number, msg).get).map(p => Commands(user).startPoll(p))),
    "/stop_poll" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.id_number, msg).get).map(p => Commands(user).stopPoll(p))),
    "/begin" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.id_number, msg).get).map(p => Commands(user).begin(p))),
    "/add_question" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.add_question, msg).get).map(p => Commands(user).addQuestion(p._1, p._2, p._3))),
    "/delete_question" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.id_number, msg).get).map(p => Commands(user).deleteQuestion(p))),
    "/answer" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.answer, msg).get).map(p => Commands(user).answer(p._1, p._2))),
    "/result" -> ((user: (Int, String, String), msg: String) => Try(ParserCombinators.parseAll
    (ParserCombinators.id_number, msg).get).map(p => Commands(user).pollResult(p))),
    "/view" -> ((user: (Int, String, String), msg: String) => Success(Commands(user).view)),
    "/list" -> ((user: (Int, String, String), msg: String) => Success(Commands(user).pollList)),
    "/end" -> ((user: (Int, String, String), msg: String) => Success(Commands(user).end))
  )
  
  def parse(message: String, privileges: String, user: Tuple3[Int, String, String]): Try[String] = {
    val name = Try(("/[^ ]+".r findAllIn message).group(0))
    val msg = message.replace(name.get + " ", "").replace("((", "#$%").replace("))", "%$#")
    name.map(n => privileges match {
      case "Administrator" => cmd(n)(user, msg)
      case "User" => n match {
        case "/create_poll" | "/delete_poll" | "/start_poll"
             | "/stop_poll" | "/add_question" | "/delete_question" =>
          Success("You don't have such level of privileges")
        case another => cmd(n)(user, msg)
      }
    }).getOrElse(Success("Unrecognised command! Say what!?"))
  }
}