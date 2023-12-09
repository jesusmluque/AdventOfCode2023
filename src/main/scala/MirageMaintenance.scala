object MirageMaintenance {

  def sumAllExtrapolatedLastValuesFrom(rawData: List[String]) = {
    sumAllExtrapolatedValuesWith(seq => seq.foldLeft(0L) { (acc, next) =>
      next.last + acc
    })(rawData)
  }
  def sumAllExtrapolatedFirstValuesFrom(rawData: List[String]) = {
    sumAllExtrapolatedValuesWith(seq => seq.foldLeft(0L) { (acc, next) =>
      next.head - acc
    })(rawData)
  }
  private def sumAllExtrapolatedValuesWith(extrapolateFunction: List[List[Long]] => Long)(rawData: List[String]) = {
    val histories = rawData.map(_.split(" ").toList.map(_.toLong))
    val total = histories.map(extrapolateFor(extrapolateFunction))
    total.sum
  }
  private def extrapolateFor(extrapolateFunction: List[List[Long]] => Long)(history: List[Long]) = {
    def reduceToZeroHistory(currentHistory: List[Long], acc: List[List[Long]]): List[List[Long]] = {
      if (acc.nonEmpty && acc.head.forall(_ == 0L))
        acc
      else
        val newHistory = calculateDifferences(currentHistory)
        reduceToZeroHistory(newHistory, newHistory :: acc)
    }
    val seq = reduceToZeroHistory(history, List(history))
    extrapolateFunction(seq)
  }
  private def calculateDifferences(history: List[Long]) = {
    def calculate(currentHistory: List[Long], previous: Long, acc: List[Long] ): List[Long] = {
      if (currentHistory.isEmpty)
        acc
      else
        calculate(currentHistory.tail, currentHistory.head, currentHistory.head - previous :: acc)
    }
    calculate(history.tail, history.head, List()).reverse
  }
}
