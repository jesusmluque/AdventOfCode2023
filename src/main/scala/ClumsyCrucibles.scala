import scala.collection.mutable

object ClumsyCrucibles {
  enum Direction(val delta: (Int,Int)):
    case North extends Direction((-1,0))
    case South extends Direction((1,0))
    case West extends Direction((0,-1))
    case East extends Direction((0,1))

  case class Node(heat: Int, direction: Direction, times: Int, pos: (Int,Int))

  def minHeatLoss(rawMap: Vector[String], maxSteps: Int = 3, minSteps: Int = 1) = {
    val map = rawMap.map(r => r.split("").toVector.map(_.toInt))
    val q = mutable.PriorityQueue.empty[Node](QueueOrdering)
    q.enqueue(Node(0, Direction.East, 1, (0,0)))
    val path = findPath(map, (map.length - 1, map(0).length - 1), q, Set(Node(0, Direction.East, 1, (0,0))), maxSteps, minSteps)
    path.map(_.heat).sum
  }

  private val QueueOrdering = new Ordering[Node] {
    def compare(a: Node, b: Node) = if a.heat < b.heat then 1 else -1
  }

  private def isInRange(map: Vector[Vector[Int]], n: (Int,Int)) =
    (n._1 >= 0 && n._1 < map.length) && (n._2 >= 0 && n._2 < map(0).length)

  private def nextPositions(map: Vector[Vector[Int]], start: Node, maxSteps: Int = 3, minSteps: Int = 1) = {
    start match
      case Node(_, Direction.East, times, pos)  =>
        val north = (pos._1 + Direction.North.delta._1, pos._2 + Direction.North.delta._2)
        val list = if isInRange(map, north) && (times >= minSteps || start.heat == 0) then List(Node(map(north._1)(north._2), Direction.North, 1, north)) else List()
        val south = (pos._1 + Direction.South.delta._1, pos._2 + Direction.South.delta._2)
        val list2 = if isInRange(map, south) && (times >= minSteps || start.heat == 0) then Node(map(south._1)(south._2), Direction.South, 1, south) :: list else list
        val east = (pos._1 + Direction.East.delta._1, pos._2 + Direction.East.delta._2)
        if isInRange(map, east) && times < maxSteps then Node(map(east._1)(east._2), Direction.East, times + 1, east) :: list2 else list2
      case Node(_, Direction.West, times, pos) =>
        val north = (pos._1 + Direction.North.delta._1, pos._2 + Direction.North.delta._2)
        val list = if isInRange(map, north) && (times >= minSteps || start.heat == 0) then List(Node(map(north._1)(north._2), Direction.North, 1, north)) else List()
        val south = (pos._1 + Direction.South.delta._1, pos._2 + Direction.South.delta._2)
        val list2 = if isInRange(map, south) && (times >= minSteps || start.heat == 0) then Node(map(south._1)(south._2), Direction.South, 1, south) :: list else list
        val west = (pos._1 + Direction.West.delta._1, pos._2 + Direction.West.delta._2)
        if isInRange(map, west) && times < maxSteps then Node(map(west._1)(west._2), Direction.West, times + 1, west) :: list2 else list2
      case Node(_, Direction.North, times, pos) =>
        val east = (pos._1 + Direction.East.delta._1, pos._2 + Direction.East.delta._2)
        val list = if isInRange(map, east) && (times >= minSteps || start.heat == 0) then Node(map(east._1)(east._2), Direction.East, 1, east) :: List() else List()
        val west = (pos._1 + Direction.West.delta._1, pos._2 + Direction.West.delta._2)
        val list2 = if isInRange(map, west) && (times >= minSteps || start.heat == 0) then Node(map(west._1)(west._2), Direction.West, 1, west) :: list else list
        val north = (pos._1 + Direction.North.delta._1, pos._2 + Direction.North.delta._2)
        if isInRange(map, north) && times < maxSteps then Node(map(north._1)(north._2), Direction.North, times + 1, north) :: list2 else list2
      case Node(_, Direction.South, times, pos) =>
        val east = (pos._1 + Direction.East.delta._1, pos._2 + Direction.East.delta._2)
        val list = if isInRange(map, east) && (times >= minSteps || start.heat == 0) then List(Node(map(east._1)(east._2), Direction.East, 1, east)) else List()
        val west = (pos._1 + Direction.West.delta._1, pos._2 + Direction.West.delta._2)
        val list2 = if isInRange(map, west) && (times >= minSteps || start.heat == 0) then Node(map(west._1)(west._2), Direction.West, 1, west) :: list else list
        val south = (pos._1 + Direction.South.delta._1, pos._2 + Direction.South.delta._2)
        if isInRange(map, south) && times < maxSteps then Node(map(south._1)(south._2), Direction.South, times + 1, south) :: list2 else list2
  }

  private def findPath(map: Vector[Vector[Int]], end: (Int,Int), toVisit: mutable.PriorityQueue[Node], visited: Set[Node], maxSteps: Int = 3, minSteps: Int = 1):Option[Node] = {
    if toVisit.isEmpty then
      None
    else
      val newPos = toVisit.dequeue()
      if newPos.pos == end && newPos.times >= minSteps then
        Some(newPos)
      else if visited.contains(Node(map(newPos.pos._1)(newPos.pos._2), newPos.direction, newPos.times, newPos.pos)) then
        findPath(map, end, toVisit, visited, maxSteps, minSteps)
      else
        val newPaths = nextPositions(map, newPos, maxSteps, minSteps)
        newPaths.foreach(a => toVisit.enqueue(Node(a.heat + newPos.heat, a.direction, a.times, a.pos)))
        findPath(map, end, toVisit, visited + Node(map(newPos.pos._1)(newPos.pos._2), newPos.direction, newPos.times, newPos.pos), maxSteps, minSteps)
  }
}
