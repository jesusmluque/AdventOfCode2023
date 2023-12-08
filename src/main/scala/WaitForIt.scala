object WaitForIt {

  def multiplyAllWaysToWinIn(races: List[(Int, Int)]) = {
    races.map { race =>
      (1 until race._1 ).map { holdTime =>
         holdTime * (race._1 - holdTime)
      }.count(_ > race._2)
    }.product
  }

  def multiplyAllWaysToWinIn2(races: List[(Long, Long)]) = {
    races.map { race =>
      (race._1 + Math.sqrt(race._1 * race._1 - 4 * race._2)/2).ceil.toLong - (race._1 - Math.sqrt(race._1 * race._1 - 4 * race._2.toLong)/2).ceil.toLong
    }.product
  }
}
