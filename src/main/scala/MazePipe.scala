import scala.annotation.tailrec

object MazePipe {
  def getTotalOfInternalTiles(rawMaze: Vector[String]) = {
    def isInsideLoop(maze: Vector[Vector[String]], candidate: (Int,Int), loop: List[(Int,Int)]) = {
      val rightRay = maze(candidate._1).zipWithIndex.takeRight(maze(0).size - candidate._2).map(tile => ((candidate._1, tile._2), tile._1))
        .filter(tile => tile._2 != "-" && loop.contains(tile._1))
        .foldLeft(("first", 0)) { (acc, g) =>
            (acc._1, g._2) match {
              case ("L","7") => ("jump", 1 + acc._2)
              case ("7","L") => ("jump", 1 + acc._2)
              case ("J","F") => ("jump", 1 + acc._2)
              case ("F","J") => ("jump", 1 + acc._2)
              case ("first", "|") => ("jump", 1 + acc._2)
              case ("|",x) => (x, 1 + acc._2)
              case ("jump", "|") => ("jump", 1 + acc._2)
              case ("first", x) => (x, acc._2)
              case ("jump", x) => (x, acc._2)
              case _ => ("jump", acc._2)
            }
      }._2
      rightRay != 0 && rightRay % 2 != 0
    }
    val maze = rawMaze.map(_.split("").toVector)
    val start = maze.zipWithIndex.foldLeft((0, 0)) { (acc, n) =>
      if n._1.contains("S") then
        (n._2, n._1.indexOf("S"))
      else
        acc
    }
    val startingDirections = List((start._1 - 1, start._2), (start._1, start._2 + 1), (start._1 + 1, start._2), (start._1, start._2 - 1))
    val path = startingDirections.map(s => findLoop(maze, s, start, Set(), List(start))).maxBy(_.size)
    val tilesIn = maze.zipWithIndex.foldLeft(List[(Int,Int)]()) { (acc, line) =>
      line._1.zipWithIndex.filter { tile =>
        !path.contains((line._2, tile._2)) && isInsideLoop(maze, (line._2, tile._2), path)
      }.map { a =>
        (line._2, a._2)
      }.toList ++ acc
    }
    tilesIn.size
  }
  def getTotalStepsOfFarthestPipe(rawMaze: Vector[String]) = {
    val maze = rawMaze.map(_.split("").toVector)
    val start = maze.zipWithIndex.foldLeft((0,0)) { (acc, n) =>
      if n._1.contains("S") then
        (n._2, n._1.indexOf("S"))
      else
        acc
    }
    val startingDirections = List((start._1 - 1, start._2), (start._1, start._2 + 1), (start._1 + 1, start._2), (start._1, start._2 - 1))
    val paths = startingDirections.map(s => findLoop(maze, s, start, Set(), List(start)))
    paths.maxBy(_.size).size / 2
  }
  @tailrec
  private def findLoop(maze: Vector[Vector[String]], start: (Int, Int), end: (Int, Int), visited: Set[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
    if (start._1 > maze.size || start._2 > maze(0).size || start._1 < 0 || start._2 < 0 || maze(start._1)(start._2) == ".")
      List()
    else if (maze(start._1)(start._2) == "S")
      acc
    else
      maze(start._1)(start._2) match {
        case "|" if acc.head._1 < start._1 => findLoop(maze, (start._1 + 1, start._2), end, visited + (start), start :: acc)
        case "|" if acc.head._1 > start._1 => findLoop(maze, (start._1 - 1, start._2), end, visited + (start), start :: acc)
        case "-" if acc.head._2 < start._2 => findLoop(maze, (start._1, start._2 + 1), end, visited + (start), start :: acc)
        case "-" if acc.head._2 > start._2 => findLoop(maze, (start._1, start._2 - 1), end, visited + (start), start :: acc)
        case "L" if acc.head._1 < start._1 => findLoop(maze, (start._1, start._2 + 1), end, visited + (start), start :: acc)
        case "L" if acc.head._2 > start._2 => findLoop(maze, (start._1 - 1, start._2), end, visited + (start), start :: acc)
        case "J" if acc.head._1 < start._1 => findLoop(maze, (start._1, start._2 - 1), end, visited + (start), start :: acc)
        case "J" if acc.head._2 < start._2 => findLoop(maze, (start._1 - 1, start._2), end, visited + (start), start :: acc)
        case "7" if acc.head._1 > start._1 => findLoop(maze, (start._1, start._2 - 1), end, visited + (start), start :: acc)
        case "7" if acc.head._2 < start._2 => findLoop(maze, (start._1 + 1, start._2), end, visited + (start), start :: acc)
        case "F" if acc.head._1 > start._1 => findLoop(maze, (start._1, start._2 + 1), end, visited + (start), start :: acc)
        case "F" if acc.head._2 > start._2 => findLoop(maze, (start._1 + 1, start._2), end, visited + (start), start :: acc)
        case _ => List()
      }
  }
}
