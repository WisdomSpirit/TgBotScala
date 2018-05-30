package bot
import parsers.CommandParser
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._
import poll.PollTimer


object TgBot extends App with TelegramBot with Polling {
  lazy val token = "579167870:AAHKuX5OQIbZ91AhAQOCimGE9KX0Vy2EZss"
  lazy val administrators : Vector[User] = Vector(
    User(299755750,isBot = false,"Ася",Some("Заостровская"),Some("Asya_zao"),Some("ru")) ,
    User(284163794,isBot = false,"Semyon",Some("Okunkov"),Some("FoOkySNick"),Some("ru-RU"))
  )

  val greeting = """Hello, my dear fellow! I'm a PollBot, it means that you can create Polls throw chatting with me!
Here is a list of commands you need to make your Poll Great again:
(Nota Bene: you nedd to be an administrator to create polls, instead, you can only answer and watch it!)
/create_poll <название_опроса:string> [анонимность:yes|no] [видимость результата:afterstop|continuous] [время начала:date] [время окончания:date]
Возвращает уникальный идентификатор опроса (Admin)
/list - Список голосований (All)
/delete_poll <идентификатор опроса:digits> - Удаление опроса (Admin)
/start_poll <идентификатор опроса:digits> - Старт процесса голосования (Admin)
/stop_poll <идентификатор опроса:digits> - Остановка процесса голосования (Admin)
/result <идентификатор опроса:digits> - Посмотреть результаты голосования (All)
/begin <идентификатор опроса:digits> - Начать работу с опросом (переключиться в контекст) (All)
/end - Закончить работу с опросом (отключиться от контекста) (All)
/view - Просмотр информации об опросе (требует выбранного опроса) (All)
/add_question <вопрос:string> [тип вопроса:open|choice|multi]
<Вариант ответа 1>
<Вариант ответа 2>
<Вариант ответа 3>
Добавление вопроса (требует выбранного опроса)
Возвращает номер добавленного вопроса (Admin)
/delete_question <номер вопроса:digits> - Удаление вопроса (требует выбранного опроса) (Admin)
/answer <номер вопроса:digits> <ответ> - Ответить на вопрос (требует выбранного опроса) (All)"""

  override def receiveMessage(msg: Message) {
    msg.text.map {
      case "/start" =>
        request(SendMessage(msg.source, greeting))
      case text =>
        msg.from.map(u =>{
          val cmd = if (administrators.contains(u))
           CommandParser.parse(text, "Administrator", (u.id, u.firstName, u.lastName.getOrElse("")))
          else CommandParser.parse(text, "User", (u.id, u.firstName, u.lastName.getOrElse("")))
          request(SendMessage(msg.source, if (cmd.isSuccess) cmd.get else "Something went wrong :("))
        })
    }
  }

  val timer = new Thread(PollTimer)
  timer.setDaemon(true)
  timer.start()
  TgBot.run()
}
