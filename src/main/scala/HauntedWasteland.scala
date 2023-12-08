import scala.annotation.tailrec

object HauntedWasteland {
  def totalStepsToReachTheEndIn(rawInstructionAndMap: List[String]) = {
    val (instructions, dictionary) = formatInstructionsAndMap(rawInstructionAndMap)
    executeInstructionsStartingAt("AAA", "ZZZ", instructions, dictionary)
  }
  def totalStepsToReachTheEndInForAllStartingPoints(rawInstructionAndMap: List[String]) = {
    val (instructions, dictionary) = formatInstructionsAndMap(rawInstructionAndMap)
    val startingNodes = dictionary.filter(node => "..A".r.matches(node._1))
    val steps = startingNodes.map(node => executeInstructionsStartingAt(node._1, "..Z", instructions, dictionary))
    val first = steps.head
    steps.tail.foldLeft(first) { (acc, next) =>
      lcm(acc, next)
    }
  }

  private def formatInstructionsAndMap(rawInstructionAndMap: List[String]) = {
    val instructions = rawInstructionAndMap.head.split("").toList
    val map = rawInstructionAndMap.tail.tail.map {
      case s"$init = ($left, $right)" => (init -> (left, right))
    }
    val dictionary = map.toMap
    (instructions, dictionary)
  }
  private def executeInstructionsStartingAt(start: String, end: String, instructions: List[String], dictionary: Map[String, (String, String)]) = {
    @tailrec
    def execute(lastInstructions: List[String], dictionary: Map[String, (String, String)], acc: Long, next: String): Long = {
      val inst = if (lastInstructions.isEmpty) instructions else lastInstructions
      if (end.r.matches(next))
        acc
      else if (inst.head == "R")
        execute(inst.tail, dictionary, acc + 1L, dictionary(next)._2)
      else
        execute(inst.tail, dictionary, acc + 1L, dictionary(next)._1)
    }
    execute(instructions, dictionary, 0, start)
  }
  private def gcd(a: Long, b: Long): Long = {
    if (b == 0)
      a
    else
      gcd(b, a % b)
  }
  private def lcm(a: Long, b: Long) = {
    a * b / gcd(a, b)
  }
}
