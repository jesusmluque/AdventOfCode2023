import scala.collection.mutable

object TheFloorWillBeLava {
  enum Direction(val delta: (Int,Int)):
    case Right extends Direction((0,1))
    case Left extends Direction((0,-1))
    case Up extends Direction((-1,0))
    case Down extends Direction((1,0))

  def getMaxTotalTilesEnergized(raw: List[String]) = {
    val contraption = raw.toVector.map(_.split("").toVector)
    val rightToLeft = contraption.indices.map(c => buildPaths(contraption, (c, 0), Direction.Right, Set()))
    val leftToRight = contraption.indices.map(c => buildPaths(contraption, (c, contraption.length - 1), Direction.Left, Set()))
    val upToDown = contraption(0).indices.map(c => buildPaths(contraption, (0, c), Direction.Down, Set()))
    val downToUp = contraption(0).indices.map(c => buildPaths(contraption, (contraption(0).length - 1, c), Direction.Up, Set()))
    List(rightToLeft.map(a => a.map(_._2).size).max, leftToRight.map(a => a.map(_._2).size).max, upToDown.map(a => a.map(_._2).size).max, downToUp.map(a => a.map(_._2).size).max).max
  }

  def getTotalTilesEnergized(raw: List[String]) = {
    val contraption = raw.toVector.map(_.split("").toVector)
    val paths = buildPaths(contraption, (0,0), Direction.Right, Set())
    paths.map(_._2).size
  }

  private def buildPaths(contraption: Vector[Vector[String]], start: (Int, Int), direction: Direction, visited: Set[(Direction, (Int, Int))]): Set[(Direction, (Int, Int))] = {
    if visited.contains(direction, start) then
      visited
    else if start._1 < 0 || start._1 >= contraption.length || start._2 < 0 || start._2 >= contraption(0).length then
      visited
    else
      contraption(start._1)(start._2) match {
        case "." => buildPaths(contraption, (start._1 + direction.delta._1, start._2 + direction.delta._2), direction, visited + ((direction, start)))
        case "/" if direction == Direction.Right => buildPaths(contraption, (start._1 + Direction.Up.delta._1, start._2 + Direction.Up.delta._2), Direction.Up, visited + ((direction, start)))
        case "/" if direction == Direction.Left => buildPaths(contraption, (start._1 + Direction.Down.delta._1, start._2 + Direction.Down.delta._2), Direction.Down, visited + ((direction, start)))
        case "/" if direction == Direction.Up => buildPaths(contraption, (start._1 + Direction.Right.delta._1, start._2 + Direction.Right.delta._2), Direction.Right, visited + ((direction, start)))
        case "/" if direction == Direction.Down => buildPaths(contraption, (start._1 + Direction.Left.delta._1, start._2 + Direction.Left.delta._2), Direction.Left, visited + ((direction, start)))
        case "\\" if direction == Direction.Right => buildPaths(contraption, (start._1 + Direction.Down.delta._1, start._2 + Direction.Down.delta._2), Direction.Down, visited + ((direction, start)))
        case "\\" if direction == Direction.Left => buildPaths(contraption, (start._1 + Direction.Up.delta._1, start._2 + Direction.Up.delta._2), Direction.Up, visited + ((direction, start)))
        case "\\" if direction == Direction.Up => buildPaths(contraption, (start._1 + Direction.Left.delta._1, start._2 + Direction.Left.delta._2), Direction.Left, visited + ((direction, start)))
        case "\\" if direction == Direction.Down => buildPaths(contraption, (start._1 + Direction.Right.delta._1, start._2 + Direction.Right.delta._2), Direction.Right, visited + ((direction, start)))
        case "-" if direction == Direction.Left || direction == Direction.Right => buildPaths(contraption, (start._1 + direction.delta._1, start._2 + direction.delta._2), direction, visited + ((direction, start)))
        case "-" =>
          val oldPath = buildPaths(contraption, (start._1 + Direction.Right.delta._1, start._2 + Direction.Right.delta._2), Direction.Right, visited + ((direction, start)))
          buildPaths(contraption, (start._1 + Direction.Left.delta._1, start._2 + Direction.Left.delta._2), Direction.Left, oldPath + ((direction, start)))
        case "|" if direction == Direction.Up || direction == Direction.Down => buildPaths(contraption, (start._1 + direction.delta._1, start._2 + direction.delta._2), direction, visited + ((direction, start)))
        case "|" =>
          val oldPath = buildPaths(contraption, (start._1 + Direction.Up.delta._1, start._2 + Direction.Up.delta._2), Direction.Up, visited + ((direction, start)))
          buildPaths(contraption, (start._1 + Direction.Down.delta._1, start._2 + Direction.Down.delta._2), Direction.Down, oldPath + ((direction, start)))
      }
  }
}
