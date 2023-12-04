object Scratchcards {
  private type Index = Int
  private type Winners = Int
  private type Repeated = Int

  def getAllPointsFor(cardsRaw: List[String]) = {
    cardsRaw.zipWithIndex.map { card =>
      calculateWinnerNumbersFor(card)._2.foldLeft(0) { (acc, _) =>
        if (acc == 0) 1 else acc * 2
      }
    }.sum
  }
  def getTotalCardsFor(cardsRaw: List[String]) = {
    def getTotalSetOfCards(initial: Vector[(Index, Winners, Repeated)], acc: Vector[(Index, Winners, Repeated)]):Vector[(Index, Winners, Repeated)] = {
      def modifyCardsAfterGame(currentCard: (Index, Winners, Repeated), offset: Int, cards: Vector[(Index, Winners, Repeated)], index: Index) = {
        cards.updated(index + offset, (cards(index + offset)._1, cards(index + offset)._2, cards(index + offset)._3 + currentCard._3))
      }
      if (initial.isEmpty)
        acc
      else
        val currentCard = initial.head
        val winners = currentCard._2
        val offset = currentCard._1
        val newCards = (1 to winners).foldLeft((acc, initial)) { (cards, index) =>
          (modifyCardsAfterGame(currentCard, offset, cards._1, index),
            modifyCardsAfterGame(currentCard, 0, cards._2, index))
        }
        getTotalSetOfCards(newCards._2.tail, newCards._1)
    }

    val initialCards = cardsRaw.zipWithIndex.map { card =>
      val winners = calculateWinnerNumbersFor(card)
      (winners._1, winners._2.size, 1)
    }
    getTotalSetOfCards(initialCards.toVector, initialCards.toVector).map(_._3).sum
  }
  private def calculateWinnerNumbersFor(card: (String, Index)) = {
    val numbersRaw = card._1.split(":")(1).split("\\|")
    val winning = numbersRaw(0).split(" ").filter(_.toLongOption.isDefined).map(_.toLong).toSet
    val myNumbers = numbersRaw(1).split(" ").filter(_.toLongOption.isDefined).map(_.toLong).toSet
    (card._2, myNumbers.intersect(winning))
  }
}
