import Repository._
import org.scalatest.FlatSpec
import commands.Commands
import poll.Poll


class TestCommands extends FlatSpec{
  val adm = "Administrator"
  val admID = 299755750
  val cmdAdm = Commands(admID, "Semyon", "Okunkov")
  val usr = "User"
  val userID = 299755678
  val cmdUsr = Commands(userID, "Someone", "Else")

  "/list" should "be empty on start" in {
    assertResult("Can You see the list of Your polls? I can't too. But they exists.") {
      cmdAdm.pollList
    }
  }

  "/create_poll" should "run well" in {
    assertResult("Success: 0") {
      cmdAdm.createPoll("name", true, "afterstop", null, null)
    }
    assertResult("Success: 1") {
      cmdAdm.createPoll("name1", false, "afterstop", null, null)
    }
    assertResult("Success: 2") {
      cmdAdm.createPoll("name(12)", true, "continuous", null, null)
    }
    assertResult("Success: 3") {
      cmdAdm.createPoll("name (ty)", true, "continuous", "12:12:12 12:12:12", null)
    }
    assertResult("Success: 4") {
      cmdAdm.createPoll("n a m e", true, "continuous", "12:12:12 12:12:11", "12:12:14 12:11:12")
    }
    assertResult("Your time is broken!") {
      cmdAdm.createPoll("n a m e", true, "continuous", "12:12:12 12:12:12", "12:12:14 12:23:12")
    }
    assertResult("Your time is broken!") {
      cmdAdm.createPoll("n a m e", true, "continuous", "12:45:12 12:12:98", null)
    }
    assertResult(5) {
      AllPolls.getAll.count(_ => true)
    }
  }

  "/list" should "show settings of all polls" in {
    assertResult("0 => name\n" +
      "is anonymous? true\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "afterstop\n" +
      "starts in: not set\n" +
      "ends in: not set\n\n" +
      "1 => name1\n" +
      "is anonymous? false\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "afterstop\n" +
      "starts in: not set\n" +
      "ends in: not set\n\n" +
      "2 => name(12)\n" +
      "is anonymous? true\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "continuous\n" +
      "starts in: not set\n" +
      "ends in: not set\n\n" +
      "3 => name (ty)\n" +
      "is anonymous? true\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "continuous\n" +
      "starts in: 12:12:12 12:12:12\n" +
      "ends in: not set\n\n" +
      "4 => n a m e\n" +
      "is anonymous? true\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "continuous\n" +
      "starts in: 12:12:12 12:12:11\n" +
      "ends in: 12:12:14 12:11:12"){
      cmdAdm.pollList
    }
  }

  "/delete_poll" should "run well" in {
    assert(AllPolls.get(1).isSuccess)
    assertResult("Exterminate! Exterminate! Exterminate!") {
      cmdAdm.deletePoll(1)
    }
    assert(AllPolls.get(1).isFailure)
    assertResult("Can't delete Your Poll, cuz there's no such one!") {
      cmdAdm.deletePoll(1)
    }
  }

  "/begin" should "start working with chosen poll" in {
    assertResult("Let's Rock!"){
      cmdAdm.begin(2)
    }
    assert(CurrentPoll.get(admID) == AllPolls.get(2))
    assertResult("You've already begun one Poll!"){
      cmdAdm.begin(3)
    }
    assertResult("There is no such Poll!"){
      cmdAdm.begin(12)
    }

    assertResult("Let's Rock!"){
      cmdUsr.begin(2)
    }
    assert(CurrentPoll.get(userID) == AllPolls.get(2))
    assertResult("You've already begun one Poll!"){
      cmdUsr.begin(3)
    }
    assertResult("There is no such Poll!"){
      cmdUsr.begin(12)
    }
  }

  "/add_question" should "add question to current poll (Admin)" in {
    assertResult("Success: 0") {
      cmdAdm.addQuestion("What're you doing here?", "choice", Vector("sleep", "eat", "work"))
    }
    assert(CurrentPoll.get(admID).get.questions.length == 1)
    assert(CurrentPoll.get(admID).get.questions(0)._1 == "What're you doing here?")
    assert(CurrentPoll.get(admID).get.questions(0)._2 == "choice")
    assert(CurrentPoll.get(admID).get.answers == Vector(Vector("sleep", "eat", "work")))
    assertResult("Success: 1") {
      cmdAdm.addQuestion("What do you want to do?", "multi", Vector("study", "train hard", "work"))
    }
    assertResult("Success: 2") {
      cmdAdm.addQuestion("What's your name?", "open", Vector())
    }
    assertResult("Success: 3") {
      cmdAdm.addQuestion("Chose one variant?", "choice", Vector("one", "1", "first"))
    }
    cmdAdm.end
    assertResult("There's no such current poll"){
      cmdAdm.addQuestion("Can't?", "open", Vector("no", "yes"))
    }
    cmdAdm.begin(2)
  }

  "/delete_question" should "delete question by id in current poll (Admin)" in {
    assert(CurrentPoll.get(admID).get.questions.length == 4)
    assertResult("Success"){
      cmdAdm.deleteQuestion(0)
    }
    assert(CurrentPoll.get(admID).get.questions.length == 3)
    assert(CurrentPoll.get(admID).get.answers(0) != Vector("sleep", "eat", "work"))
    assertResult("Can't delete this Question, set the right Poll or question!"){
      cmdAdm.deleteQuestion(3)
    }
  }

  "/end" should "stop working with current poll" in {
    cmdAdm.begin(2)
    assertResult("Now, you're free!"){
      cmdAdm.end
    }
    assertResult("You have no begun Poll!"){
      cmdAdm.end
    }

    assertResult("Now, you're free!"){
      cmdUsr.end
    }
    assertResult("You have no begun Poll!"){
      cmdUsr.end
    }
  }

  "/start_poll" should "run well" in { //пока нет условия про начальное время
    assert(AllPolls.getAllRun.isEmpty)
    assertResult("Your poll was just started, look for feedback!") {
      cmdAdm.startPoll(2)
    }
    assert(AllPolls.getAllRun.nonEmpty)
    assertResult("Can't start your Poll, cuz there's no such one!") {
      cmdAdm.startPoll(1)
    }
    assertResult("The Poll will start itself when the time come! Wait..."){
      cmdAdm.startPoll(3)
    }
  }

  "/answer" should "save the answer on question by id" in {
    cmdAdm.begin(2)
    cmdUsr.begin(2)

    {
      assertResult("Type of the question - Multi, your answer is incorrect") {
        cmdAdm.answer(0, "3 4")
      }
      assertResult("Type of the question - Multi, your answer is incorrect") {
        cmdAdm.answer(0, "0, 1")
      }

      assertResult("Success") { //ответы на вопрос типа multi
        cmdAdm.answer(0, "0 1")
      }
      assert(Inner.get(CurrentPoll.get(admID).get.id, 0, 0) == Vector(((admID, "Semyon", "Okunkov"), "")))
      assert(Inner.get(CurrentPoll.get(admID).get.id, 0, 1) == Vector(((admID, "Semyon", "Okunkov"), "")))

      assertResult("You've already answered it!") { //повторно отвечать нельзя
        cmdAdm.answer(0, "2")
      }

      assertResult("Success") { //ответы на вопрос типа multi
        cmdUsr.answer(0, "0 2")
      }
      assert(Inner.get(CurrentPoll.get(admID).get.id, 0, 0) == Vector(((admID, "Semyon", "Okunkov"), ""),
        ((userID, "Someone", "Else"), "")))
      assert(Inner.get(CurrentPoll.get(admID).get.id, 0, 2) == Vector(((userID, "Someone", "Else"), "")))

      assertResult("You've already answered it!") { //повторно отвечать нельзя
        cmdUsr.answer(0, "0")
      }
    }

    {
      assertResult("Type of the question - Open, your answer is incorrect") { //нужен ответ для вопоса open
        cmdAdm.answer(1, "")
      }
      assertResult("Success") { //норм ответ на open
        cmdAdm.answer(1, "Admin")
      }
      assert(Inner.get(CurrentPoll.get(admID).get.id, 1, 0) == Vector(((admID, "Semyon", "Okunkov"), "Admin")))

      assertResult("You've already answered it!") { //повторно отвечать нельзя
        cmdAdm.answer(1, "0dffs")
      }
      assertResult("Success") {
        cmdUsr.answer(1, "User")
      }
      assert(Inner.get(CurrentPoll.get(userID).get.id, 1, 0) == Vector(((admID, "Semyon", "Okunkov"), "Admin"),
        ((userID, "Someone", "Else"), "User")))

      assertResult("You've already answered it!") { //повторно отвечать нельзя
        cmdUsr.answer(1, "0dfswe")
      }
    }

    {
      assertResult("Type of the question - Choice, your answer is incorrect") {
        cmdAdm.answer(2, "1 2")
      }
      assertResult("Type of the question - Choice, your answer is incorrect") {
        cmdAdm.answer(2, "4")
      }
      assertResult("Type of the question - Choice, your answer is incorrect") {
        cmdAdm.answer(2, "1!")
      }
      assertResult("Success") {
        cmdAdm.answer(2, "1")
      }
      assert(Inner.get(CurrentPoll.get(admID).get.id, 2, 1) == Vector(((admID, "Semyon", "Okunkov"), "")))

      assertResult("Success") {
        cmdUsr.answer(2, "1")
      }
      assert(Inner.get(CurrentPoll.get(admID).get.id, 2, 1) == Vector(((admID, "Semyon", "Okunkov"), ""),
        ((userID, "Someone", "Else"), "")))

      assertResult("You've already answered it!") {
        cmdAdm.answer(2, "1")
      }
      assertResult("You've already answered it!") {
        cmdUsr.answer(2, "1")
      }
    }
    cmdAdm.end
    cmdUsr.end
  }

  "/stop_poll" should "run well" in { //нет условия про конечное время
    assert(AllPolls.getRun(2).isSuccess)
    assertResult("Your poll was just finished, that was a great poll!") {
      cmdAdm.stopPoll(2)
    }
    assert(AllPolls.getRun(2).isFailure)
    assertResult("Cant't stop Your Poll, cuz it's not run!") {
      cmdAdm.stopPoll(2)
    }
    AllPolls.setRun(4)
    assertResult("The Poll will stop itself when the time come! Wait...") {
      cmdAdm.stopPoll(4)
    }
  }

  "/view" should "show current poll with its questions and answers" in {
    assertResult("There's no current Poll!") {
      cmdUsr.view
    }

    val poll = Poll(1, "NNNName", true, "afterstop", null, null, false, false, Vector(("Qchoice?", "choice"),
      ("Qmulti?", "multi"), ("Qopen?", "open")), Vector(Vector("ch1", "ch2"), Vector("m1", "m2", "m3"), Vector()))
    CurrentPoll.set(userID, poll)
    assertResult("Poll №1 NNNName:\n" +
      "0. Qchoice? (choice):\n\t0) ch1\n\t1) ch2\n" +
      "1. Qmulti? (multi):\n\t0) m1\n\t1) m2\n\t2) m3\n" +
      "2. Qopen? (open):\n"){
      cmdUsr.view
    }
  }

  "/result" should "show the results of chosen poll" in {
    assertResult("We don't have such a Poll") {
      cmdAdm.pollResult(12)
    }
    assertResult("You can view the result only when the poll will be over") {
      cmdAdm.pollResult(0)
    }
    assertResult("Poll №2 name(12):\n" +
      "0. What do you want to do? (multi):\n\t" +
      "0) study - 2 votes\n\t1) train hard - 1 votes\n\t2) work - 1 votes\n" +
      "1. What's your name? (open):\n\tAdmin\n\tUser\n" +
      "2. Chose one variant? (choice):\n\t0) one - 0 votes\n\t1) 1 - 2 votes\n\t2) first - 0 votes"){
      cmdAdm.pollResult(2) //continuous + anonymous
    }
  }
}