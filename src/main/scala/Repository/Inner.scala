package Repository

object Inner {
  private var inner : Map[(Int, Int, Int), Vector[((Int, String, String), String)]] =
    Map[(Int, Int, Int), Vector[((Int, String, String), String)]]()
  //                      pollId  numQ numA       people answer

  def get(pollId: Int, numQ: Int, numA: Int): Vector[((Int, String, String), String)]= {
    inner.getOrElse((pollId, numQ, numA), Vector())
  }

  def set(pollId: Int, numQ: Int, numA: Int, user: (Int, String, String), answer: String): Unit ={
    val list : Vector[((Int, String, String), String)] =
      inner.getOrElse((pollId, numQ, numA), Vector()) :+ (user, answer)
    inner = inner updated ((pollId, numQ, numA), list)
  }
}