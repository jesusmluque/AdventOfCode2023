import java.lang

object CunundrumCube:

  def sumAllValidGamesIdsIn(games: List[String]) = {
    games.map { line =>
      reduceGame(line)
    }.filter { game =>
      game._2.forall { g =>
        (g._2 == "red" && g._1 <= 12L) || (g._2 == "blue" && g._1 <= 14L) || (g._2 =="green" && g._1 <= 13L)
      }
    }.foldLeft(0L) { (acc, n) =>
      acc + n._1.toLong
    }
  }

  def sumOfPowerForEachGameIn(games: List[String]) = {
    val idRegExp = "Game ([0-9]+):.*".r
    games.map { line =>
      reduceGame(line)._2.groupBy(_._2).map { n =>
        (n._1, n._2.maxBy(_._1))
      }.foldLeft(1L) { (acc, n) =>
        acc * n._2._1
      }
    }.sum
  }

  private def reduceGame(line: String) = {
    val idRegExp = "Game ([0-9]+):.*".r
    val idRegExp(id) = line
    val gameReduced = line.split(":")(1).split(";").map(_.split(",")).flatMap { t =>
      t.map { m =>
        val r = " *([0-9]+) (blue|red|green).*".r
        val r(q, c) = m
        (q.toLong, c)
      }
    }
    (id, gameReduced)
  }
