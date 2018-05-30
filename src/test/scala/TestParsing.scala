import parsers.CommandParser._
import org.scalatest._

class TestParsing extends FlatSpec {
  val adm = "Administrator"
  val admin = (299755750, "Asya", "Zaostrovskaya")
  val usr = "User"
  val user = (299755678, "Someone", "Else")

  "/list" should "be parsed right" in {
    assert(parse("/list", adm, admin).isSuccess)
    assert(parse("/list", usr, user).isSuccess)
  }

  "/create_poll" should "be parsed right (Admin)" in {
    assert(parse("/create_poll (name0)", adm, admin).isSuccess)
    assert(parse("/create_poll (name ((1))) (yes)", adm, admin).isSuccess)
    assert(parse("/create_poll (n a m e 2) (no) (afterstop)", adm, admin).isSuccess)
    assert(parse("/create_poll (na((me)) 3) (yes) (continuous) (23:23:23 23:23:23)", adm, admin).isSuccess)
    assert(parse("/create_poll (name4) (yes) (continuous) (23:23:23 23:23:23) (25:25:25 25:25:25)", adm, admin).isSuccess)
    assert(parse("/create_poll (name ((1))) (ys)", adm, admin).isFailure)
    assert(parse("/create_poll (n a m e 2) (no) (aftop)", adm, admin).isFailure)
    assert(parse("/create_poll (na((me)) 3) (yes) (continuous) (23:23:23 23:2:23)", adm, admin).isFailure)
  }

  "Common User" should "have no privileges for some commands" in {
    assertResult("You don't have such level of privileges"){
      parse("/create_poll (name0)", usr, user).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/delete_poll (1)", usr, user).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/start_poll (0)", usr, user).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/stop_poll (0)", usr, user).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/add_question (asdfhj) (fghj)", usr, user).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/delete_question (0)", usr, user).get
    }
  }

  "CommandParser" should "doesn't know strange input" in {
    assertResult("Unrecognised command! Say what!?") {
      parse("/creat_pol (qwerty)", adm, admin).get
    }
  }

  "/delete_poll" should "be parsed right (Admin)" in {
    assert(parse("/delete_poll (0)", adm, admin).isSuccess)
    assert(parse("/delete_poll ", adm, admin).isFailure)
    assert(parse("/delete_poll ()", adm, admin).isFailure)
    assert(parse("/delete_poll (d)", adm, admin).isFailure)
  }

  "/begin" should "be parsed right" in {
    assert(parse("/begin (a)", adm, admin).isFailure)
    assert(parse("/begin (3)", adm, admin).isSuccess)
    assert(parse("/begin (4)", usr, user).isSuccess)
  }

  "/add_question" should "add question to current poll (Admin)" in {
    assert(parse("/add_question (new question?) (open)" +
      """
ans1
ans2
asn3""", adm, admin).isFailure)
    assert(parse("/add_question (question?) (multi)", adm, admin).isFailure)
    assert(parse("/add_question (question2?) (choice)", adm, admin).isFailure)
    assert(parse("/add_question (new question?) (wrong) \nans1\nans2", adm, admin).isFailure)
    assert(parse("/add_question", adm, admin).isFailure)

    assert(parse("/add_question (What's up?) (open)", adm, admin).isSuccess)
    assert(parse("/add_question (choice question?) (choice)" +
      """
ans1
ans2
ans3""", adm, admin).isSuccess)
    assert(parse("/add_question (multi question?) (multi)" +
      """
ans1
ans2
ans3""", adm, admin).isSuccess)
  }

  "/delete_question" should "be parsed right (Admin)" in {
    assert(parse("/delete_question (a)", adm, admin).isFailure)
    assert(parse("/delete_question", adm, admin).isFailure)
    assert(parse("/delete_question (0)", adm, admin).isSuccess)
  }

  "/end" should "be parsed right" in {
    assert(parse("/end", adm, admin).isSuccess)
    assert(parse("/end", usr, user).isSuccess)
  }

  "/start_poll" should "be parsed right (Admin)" in {
    assert(parse("/start_poll (1)", adm, admin).isSuccess)
    assert(parse("/start_poll ()", adm, admin).isFailure)
    assert(parse("/start_poll (d)", adm, admin).isFailure)
    assert(parse("/start_poll", adm, admin).isFailure)
  }

  "/answer" should "be parsed right" in {

    assert(parse("/answer", adm, admin).isFailure)
    assert(parse("/answer (j)", adm, admin).isFailure)

    assert(parse("/answer", usr, user).isFailure)
    assert(parse("/answer (jk)", usr, user).isFailure)

    assert(parse("/answer (0) (1)", adm, admin).isSuccess)
    assert(parse("/answer (0) (1 2)", adm, admin).isSuccess)
    assert(parse("/answer (0) (answer)", adm, admin).isSuccess)

    assert(parse("/answer (0) (1)", usr, user).isSuccess)
    assert(parse("/answer (0) (1 2)", usr, user).isSuccess)
    assert(parse("/answer (0) (answer)", usr, user).isSuccess)
  }

  "/stop_poll" should "be parsed right (Admin)" in {
    assert(parse("/stop_poll (1)", adm, admin).isSuccess)
    assert(parse("/stop_poll ()", adm, admin).isFailure)
    assert(parse("/stop_poll (d)", adm, admin).isFailure)
    assert(parse("/stop_poll", adm, admin).isFailure)
  }

  "/view" should "be parsed right" in {
    assert(parse("/view", adm, admin).isSuccess)
    assert(parse("/view", usr, user).isSuccess)
  }

  "/result" should "be parsed right" in {
    assert(parse("/result (1)", adm, admin).isSuccess)
    assert(parse("/result (12)", usr, user).isSuccess)
    assert(parse("/result (h)", adm, admin).isFailure)
    assert(parse("/result (sd)", usr, user).isFailure)
    assert(parse("/result (1h)", adm, admin).isFailure)
    assert(parse("/result", adm, admin).isFailure)
    assert(parse("/result ()", adm, admin).isFailure)
  }
}