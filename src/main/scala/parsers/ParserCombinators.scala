package parsers
import scala.util.Properties
import scala.util.matching.Regex
import scala.util.parsing.combinator._

object ParserCombinators extends RegexParsers {
  override val whiteSpace: Regex = " +".r

  def id_number : Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ (_.toInt)

  def question : Parser[String] = "(" ~> "[^)]+".r <~ ")"
  def question_type : Parser[String] = "(" ~> "(open)|(choice)|(multi)".r <~ ")"
  def answers : Parser[String] = (Properties.lineSeparator | "\n") ~> ".+".r
  def add_question : Parser[(String, String, Vector[String])] =
    question ~ question_type.? ~ rep(answers) ^^
      (expr => {
        val question = expr._1._1.replace("#$%", "(").replace("%$#", ")")
        val qtype = if (expr._1._2.nonEmpty) expr._1._2.head else "open"
        val answer = expr._2.toVector
        if (qtype == "open" && answer.nonEmpty) throw new Exception()
        if (qtype != "open" && answer.isEmpty) throw  new Exception()
        (question, qtype, answer)})

  def answer : Parser[(Int, String)] = id_number ~ ("(" ~> "[^)]+".r <~ ")") ^^ (expr => (expr._1, expr._2))

  def poll_name : Parser[String] = "(" ~> "[^)]+".r <~ ")"
  def anonymous : Parser[Boolean] = "(" ~> "(yes)|(no)".r <~ ")" ^^ (expr => expr == "yes")
  def view_type : Parser[String] = "(" ~> "(afterstop)|(continuous)".r <~ ")"
  def time : Parser[String] = "(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")"
  def create_poll : Parser[(String, Boolean, String, String, String)] =
    poll_name ~ anonymous.? ~ view_type.? ~ time.? ~ time.? ^^
      (expr => {
        val name = expr._1._1._1._1.replace("#$%", "(").replace("%$#", ")")
        val anonymous = if (expr._1._1._1._2.nonEmpty) expr._1._1._1._2.head else true
        val vew_type = if (expr._1._1._2.nonEmpty) expr._1._1._2.head else "afterstop"
        val time_start = if (expr._1._2.nonEmpty) expr._1._2.head else null
        val time_stop = if (expr._2.nonEmpty) expr._2.head else null
        (name, anonymous, vew_type, time_start, time_stop)
      })
}
