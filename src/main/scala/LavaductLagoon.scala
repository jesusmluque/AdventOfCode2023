import scala.collection.mutable

object LavaductLagoon {
  type Point = (Int,Int)
  type Trench = (Point, String)
  case class Map(points: List[Trench], perimeter: Long) {
    val topL: Point = (points.minBy(_._1._1)._1._1, points.minBy(_._1._2)._1._2)
    val topR: Point = (points.minBy(_._1._1)._1._1, points.maxBy(_._1._2)._1._2)
    val downL: Point = (points.maxBy(_._1._1)._1._1, points.minBy(_._1._2)._1._2)
    val downR: Point = (points.maxBy(_._1._1)._1._1, points.maxBy(_._1._2)._1._2)
    def isInside(p: Point): Boolean = {
        (p._1 to topL._1 by -1).map(a => (a, p._2)).exists(a => points.exists(b => b._1 == a)) &&
        (p._1 to downL._1).map(a => (a, p._2)).exists(a => points.exists(b => b._1 == a)) &&
          (p._2 to topR._2).map(a => (p._1, a)).exists(a => points.exists(b => b._1 == a)) &&
            (p._2 to topL._2 by -1).map(a => (p._1, a)).exists(a => points.exists(b => b._1 == a))
    }
    def volume = {
      def flooding(visited: Set[Point], toVisit: mutable.Queue[Point], acc: List[Point]): List[Point] = {
        if toVisit.isEmpty then
          acc
        else
          val next = toVisit.dequeue
          if !visited.contains(next) && !points.exists(_._1 == next) then
            List((next._1 + 1, next._2), (next._1, next._2 + 1), (next._1 - 1, next._2), (next._1, next._2 - 1)).foreach(toVisit.enqueue(_))
            flooding(visited + next, toVisit, next :: acc)
          else
            flooding(visited, toVisit, acc)
      }
      val first = firstInside
      val all = flooding(Set(), mutable.Queue(first._1), List())
      all.size + points.size
    }
    def firstInside = {
      (topL._1 to downL._1).foldLeft(((0,0), false)) { (acc, row) =>
        if acc._2 then
          acc
        else
          (topL._2 to topR._2).map((row, _)).find(p => isInside(p) && !points.exists(_._1 == p)) match
            case None => acc
            case Some(p) => (p, true)
      }
    }
    def volumeShoelace = {
      val vertices = points.map(_._1)
      Math.abs(vertices.tail.foldLeft((vertices.head, 0D)) { (acc, v2) =>
        val v1 = acc._1
        (v2, v1._1.toDouble * v2._2.toDouble - v1._2.toDouble * v2._1.toDouble + acc._2)
      }._2 / 2D).toLong + perimeter / 2L + 1
    }
  }
  object Map {
    def apply(instructions: List[(String, Int, String)]):Map = {
      val edges = instructions.foldLeft(List[((Int, Int), String)]()) { (acc, next) =>
        val newAcc = if acc.isEmpty then ((0, 0), next._3) :: acc else acc
        val lastPoint = newAcc.head
        val newPoints = next._1 match
          case "U" => (1 to next._2).map(count => ((lastPoint._1._1 - count, lastPoint._1._2), next._3))
          case "D" => (1 to next._2).map(count => ((lastPoint._1._1 + count, lastPoint._1._2), next._3))
          case "R" => (1 to next._2).map(count => ((lastPoint._1._1, lastPoint._1._2 + count), next._3))
          case "L" => (1 to next._2).map(count => ((lastPoint._1._1, lastPoint._1._2 - count), next._3))
        newPoints.reverse.toList ++ acc
      }
      Map(edges, instructions.map(_._2.toLong).sum)
    }
  }
  def calculateTotalVolumeFor(rawData: List[String]) = {
    val instructions = rawData.map {
      case s"${direction} ${steps} (${color})" => (direction, steps.toInt, color)
    }
    val map = Map(instructions)
    map.volumeShoelace
  }

  def calculateTotalVolumeFor2(rawData: List[String]) = {
    val instructions = rawData.map {
      case s"${_} ${_} (${color})" => val (value, dir) = color.splitAt(1)._2.splitAt(color.length - 2)
        dir match
          case "0" => ("R", Integer.parseInt(value, 16))
          case "1" => ("D", Integer.parseInt(value, 16))
          case "2" => ("L", Integer.parseInt(value, 16))
          case "3" => ("U", Integer.parseInt(value, 16))
    }
    val vertices = instructions.foldLeft(List((0,0))) { (acc, inst) =>
      val newPoint = inst match
        case ("R", value) => (acc.head._1, acc.head._2 + value)
        case ("D", value) => (acc.head._1 + value, acc.head._2)
        case ("L", value) => (acc.head._1, acc.head._2 - value)
        case ("U", value) => (acc.head._1 - value, acc.head._2)
      newPoint :: acc
    }
    val perimeter = instructions.map(_._2.toLong).sum
    shoelaceAreaAlgorithm(vertices, perimeter)
  }

  private def shoelaceAreaAlgorithm(vertices: List[(Int, Int)], perimeter: Long) = {
    Math.abs(vertices.tail.foldLeft((vertices.head, 0D)) { (acc, v2) =>
      val v1 = acc._1
      (v2, v1._1.toDouble * v2._2.toDouble - v1._2.toDouble * v2._1.toDouble + acc._2)
    }._2 / 2D).toLong + perimeter / 2L + 1
  }
}
