object CosmicExpansion {
  def sumAllShortestPath(universeRaw: Vector[String], expansionFactor:Long = 2L) = {
    def sumOfDistances(stars: List[(Long,Long)], acc: Long):Long = {
      if stars.isEmpty then
        acc
      else
        val star = stars.head
        val sum = stars.foldLeft(0L) { (a, s) =>
          Math.abs(star._1 - s._1) + Math.abs(star._2 - s._2) + a
        } + acc
        sumOfDistances(stars.tail, sum)
    }

    def expandUniverseInOneDimension(stars: List[((Long,Long), (Long,Long))]) = {
      val emptyRowsV = (0L until universeRaw.size.toLong).toSet.diff(stars.map(_._1._1).toSet)
      val starsExpandedVertically = emptyRowsV.foldLeft(stars) { (acc, emptyRow) =>
        acc.map { star =>
          if (star._1._1 > emptyRow)
            (star._1, (star._2._1 + expansionFactor - 1L, star._2._2))
          else
            star
        }
      }
      starsExpandedVertically
    }

    val stars = universeRaw.toList.zipWithIndex.map(a => (a._1, a._2.toLong)).flatMap { row =>
      row._1.split("").zipWithIndex.filter(_._1 == "#").map(space => (row._2, space._2.toLong)).toList
    }
    val initialOldNewStars = stars.map(a => (a, a))
    val starsExpandedVerticallyRotated = expandUniverseInOneDimension(initialOldNewStars).map(s => ((s._1._2, s._1._1), (s._2._2, s._2._1)))
    val starsExpanded = expandUniverseInOneDimension(starsExpandedVerticallyRotated).map(s => ((s._1._2, s._1._1), (s._2._2, s._2._1)))

    sumOfDistances(starsExpanded.map(_._2), 0)
  }
}
