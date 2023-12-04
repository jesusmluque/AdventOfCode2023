object GearRatios {

  def sumAllGears(engineRaw: Vector[String]) = {
    val engine = parseEngine(engineRaw)
    val symbolPositions = findSymbolsPositions(engine, n => n._1 == "*")
    symbolPositions.values.flatMap { position =>
      position.map(p => findPartsAroundPosition(engine, p)._1).filter(l => l.length == 2)
    }.map(m => m.product.toLong).sum
  }

  def sumAllParts(engineRaw: Vector[String]) = {
    val engine = parseEngine(engineRaw)
    val symbolPositions = findSymbolsPositions(engine, n => n._1.toIntOption.isEmpty && n._1 != ".")
    symbolPositions.values.toList.flatMap { symbolPosition =>
      symbolPosition.map(position => findPartsAroundPosition(engine, position)._1)
    }.flatten.map(_.toLong).sum
  }

  private def parseEngine(engineRaw: Vector[String]) = {
    engineRaw.map { rawLine =>
      rawLine.split("")
    }
  }

  private def findSymbolsPositions(engine: Vector[Array[String]], filter: ((String, Int)) => Boolean) = {
    engine.zipWithIndex.foldLeft(Map[String, List[(Int, Int)]]()) { (acc, row) =>
      row._1.zipWithIndex.filter(filter).foldLeft(acc) { (a, n) =>
        a.get(n._1) match {
          case Some(symbol) => a.updated(n._1, ((row._2, n._2)) :: a(n._1))
          case None => a + ((n._1, List((row._2, n._2))))
        }
      }
    }
  }

  private def findPartsAroundPosition(engine: Vector[Array[String]], position: (Int, Int)) = {
    def buildPartOfNumber(range: Range, position: (Int, Int), reverse: Boolean) =
      val nonFormatted = range.map(engine(position._1)(_)).takeWhile(_.toIntOption.isDefined)
      if (reverse) nonFormatted.reverse.mkString("") else nonFormatted.mkString("")
    def calculateTargetPositions = {
      List((position._1 - 1, position._2),
        (position._1 - 1, position._2 + 1),
        (position._1, position._2 + 1),
        (position._1 + 1, position._2 + 1),
        (position._1 + 1, position._2),
        (position._1 + 1, position._2 - 1),
        (position._1, position._2 - 1),
        (position._1 - 1, position._2 - 1))
        .filter(p => p._1 >= 0 && p._2 >= 0 && p._1 < engine.length && p._2 < engine(0).length)
    }
    calculateTargetPositions.foldLeft((List[Int](), Set[(Int, Int)]())) { (acc, p) =>
      if (!acc._2.contains(p) && engine(p._1)(p._2).toIntOption.isDefined)
        val numLeftPart = buildPartOfNumber(p._2 to 0 by -1, p, true)
        val numRightPart = buildPartOfNumber(p._2 + 1 until engine(0).length, p, false)
        val number = (numLeftPart + numRightPart).toInt
        val positionsAlreadyScannedLeft = List.fill(numLeftPart.length)(p._1).zip(p._2 to p._2 - numLeftPart.length by -1).toSet
        val positionsAlreadyScannedRight = List.fill(numRightPart.length)(p._1).zip(p._2 + 1 to p._2 + numRightPart.length).toSet
        (number :: acc._1, (acc._2 ++ positionsAlreadyScannedLeft) ++ positionsAlreadyScannedRight)
      else
        acc
    }
  }
}
