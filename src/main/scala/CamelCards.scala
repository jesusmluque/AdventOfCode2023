object CamelCards {

  class Hand(raw: String, val withJs: Boolean = false):
    private val values = raw.split("").toList
    private def getType = {
      val newValues = if (withJs) {
        val jays = this.values.count(_ == "J")
        val intermediate = this.values.filter(_ != "J").toSet.map { value =>
          (value, this.values.count(_ == value))
        }
        val max = intermediate.toList match {
          case List() => "J"
          case a => a.maxBy(_._2)._1
        }
        this.values.map { c =>
          if (c == "J")
            max
          else
            c
        }
      } else {
        this.values
      }
      val intermediateWithJs = newValues.toSet.map { value =>
        (value, newValues.count(_ == value))
      }
      intermediateWithJs.toList match {
        case List(a) => 7
        case List(a, b) if a._2 == 4 || b._2 == 4 => 6
        case List(a, b) if a._2 == 3 || b._2 == 3 => 5
        case List(a, b, c) if a._2 == 3 || b._2 == 3 || c._2 == 3 => 4
        case List(a, b, c) if a._2 == 1 || b._2 == 1 || c._2 == 1 => 3
        case List(a, b, c, d) if a._2 == 2 || b._2 == 2 || c._2 == 2 || d._2 == 2 => 2
        case List(a, b, c, d, e) => 1
      }
    }

  object Hand:
    private val labels = Vector("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2").zipWithIndex.toMap
    private val labelsWithJs = Vector("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J").zipWithIndex.toMap

    def compare(hand1: Hand, hand2: Hand) = {
        val hand1Type = hand1.getType
        val hand2Type = hand2.getType
        val labelsToApply = if (hand1.withJs) labelsWithJs else labels
        if (hand1Type > hand2Type)
          1
        else if (hand1Type < hand2Type)
          -1
        else
          val (card1, card2) = hand1.values.zip(hand2.values).filter { h =>
            h._1 != h._2
          }.head
          if (labelsToApply(card1) < labelsToApply(card2))
            1
          else
            -1
      }

  def calculateTotalWinningFor(handsRaw: List[String]) = {
    calculate(handsRaw)
  }

  def calculateTotalWinningWithJsFor(handsRaw: List[String]) = {
    calculate(handsRaw, true)
  }

  private def calculate(handsRaw: List[String], withJs: Boolean = false) = {
    def buildHand(r: String) = Hand(r, withJs)
    handsRaw.map { raw =>
        val parts = raw.split(" ")
        (parts(0), parts(1).toLong)
      }.map(h => (buildHand(h._1), h._2)).sortWith((h1, h2) => Hand.compare(h1._1, h2._1) > 0).reverse.map(_._2)
      .zipWithIndex.map(h => (h._1, h._2 + 1)).foldLeft(0L) { (acc, n) =>
        n._2 * n._1 + acc
      }
  }
}
