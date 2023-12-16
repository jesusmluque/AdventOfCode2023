object ParabolicReflectorDish {

  /*
  Cycle detected at position start of 107, cycle of 9 numbers
  102512
  102509
  102505
  102500
  102498
  102507
  102504
  102497
  102507
  (1000000000 - 107) % 9 = 2 -> 102509
   */
  def totalLoadAfterCycles(platform: Vector[String], cycles: Long) = {
    def calculateTilt(rocks: List[(String, (Int, Int))], f: ((String, (Int, Int)), List[(String, (Int, Int))]) => (String, (Int, Int))) = {
      rocks.foldLeft(List[(String, (Int, Int))]()) { (acc, rock) =>
        if rock._1 == "#" then
          rock :: acc
        else
          f(rock, acc) :: acc
      }.reverse
    }
    def tiltEast(rocks: List[(String, (Int, Int))]) = {
      def newPosition(rock: (String, (Int, Int)), newRocks: List[(String, (Int, Int))]): (String, (Int, Int)) = {
        val sameRow = rocks.filter(r => r._2._1 == rock._2._1 && r != rock)
        if rock._1 == "O" && (sameRow.contains(("#", rock._2)) || newRocks.contains(("O", rock._2))) then
          (rock._1, (rock._2._1, rock._2._2 - 1))
        else if rock._1 == "O" && rock._2._2 == platform(0).length - 1 then
          (rock._1, rock._2)
        else
          newPosition((rock._1, (rock._2._1, rock._2._2 + 1)), newRocks)
      }
      calculateTilt(rocks, newPosition)
    }

    def tiltWest(rocks: List[(String, (Int, Int))]) = {
      def newPosition(rock: (String, (Int, Int)), newRocks: List[(String, (Int, Int))]): (String, (Int, Int)) = {
        val sameRow = rocks.filter(r => r._2._1 == rock._2._1 && r != rock)
        if rock._1 == "O" && (sameRow.contains(("#", rock._2)) || newRocks.contains(("O", rock._2))) then
          (rock._1, (rock._2._1, rock._2._2 + 1))
        else if rock._1 == "O" && rock._2._2 == 0 then
          rock
        else
          newPosition((rock._1, (rock._2._1, rock._2._2 - 1)), newRocks)
      }
      calculateTilt(rocks, newPosition)
    }

    def tiltUp(rocks: List[(String, (Int, Int))]) = {
      def newPosition(rock: (String, (Int, Int)), newRocks: List[(String, (Int, Int))]): (String, (Int, Int)) = {
        val sameColumns = rocks.filter(r => r._2._2 == rock._2._2 && r != rock)
        if rock._1 == "O" && (sameColumns.contains(("#", rock._2)) || newRocks.contains(("O", rock._2))) then
          (rock._1, (rock._2._1 + 1, rock._2._2))
        else if rock._1 == "O" && rock._2._1 == 0 then
          rock
        else
          newPosition((rock._1, (rock._2._1 - 1, rock._2._2)), newRocks)
      }
      calculateTilt(rocks, newPosition)
    }

    def tiltDown(rocks: List[(String, (Int, Int))]) = {
      def newPosition(rock: (String, (Int, Int)), newRocks: List[(String, (Int, Int))]): (String, (Int, Int)) = {
        val sameColumns = rocks.filter(r => r._2._2 == rock._2._2 && r != rock)
        if rock._1 == "O" && (sameColumns.contains(("#", rock._2)) || newRocks.contains(("O", rock._2))) then
          (rock._1, (rock._2._1 - 1, rock._2._2))
        else if rock._1 == "O" && rock._2._1 == platform.length - 1 then
          rock
        else
          newPosition((rock._1, (rock._2._1 + 1, rock._2._2)), newRocks)
      }
      calculateTilt(rocks.sortBy(_._2._1)(Ordering.Int.reverse), newPosition)
    }
    def sumReflectors(finalRocks: List[(String, (Int, Int))]) = {
      finalRocks.groupBy(_._2._2).map { l =>
        l._2.foldLeft(0) { (a, n) =>
          if n._1 == "O" then
            a + platform.length - n._2._1
          else
            a
        }
      }.sum
    }

    val rocks = platform.zipWithIndex.foldLeft(List[(String, (Int, Int))]()) { (acc, row) =>
      row._1.split("").zipWithIndex.toList.foldLeft(acc) { (a, place) =>
        if place._1 == "O" || place._1 == "#" then
          (place._1, (row._2, place._2)) :: a
        else
          a
      }
    }
    val finalRocks = (1L to cycles).foldLeft(rocks) { (acc, n) =>
      val up = tiltUp(acc.sortBy(_._2._2).groupBy(_._2._2).toSeq.sortBy(_._1).map(_._2).flatMap(a => a.sortBy(_._2._1)).toList)
      val west = tiltWest(up)
      val down = tiltDown(west.sortBy(_._2._2))
      val east = tiltEast(down.sortBy(_._2._2)(Ordering.Int.reverse))
      east
    }
    sumReflectors(finalRocks)
  }

  def totalLoadAfterNorthTilt(platform: Vector[String]) = {
    switchRwoByColums(platform).map { row =>
      row.split("").zipWithIndex.foldLeft((0, row.length)) { (acc, n) =>
        n match {
          case (".", _) => acc
          case ("O", _) => (acc._1 + acc._2, acc._2 - 1)
          case ("#", i) => (acc._1, row.length - (i + 1))
        }
      }
    }.map(_._1).sum
  }

  private def switchRwoByColums(notes: Vector[String]) = {
    notes.zipWithIndex.foldLeft(Map[Int, String]()) { (acc, row) =>
      row._1.split("").zipWithIndex.foldLeft(acc) { (a, element) =>
        if a.contains(element._2) then
          a.updated(element._2, a(element._2) + element._1)
        else
          a.updated(element._2, element._1)
      }
    }.toList.sortBy(_._1).map(_._2)
  }
}