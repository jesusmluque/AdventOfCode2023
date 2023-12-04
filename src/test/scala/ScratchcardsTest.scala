import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class ScratchcardsTest extends AnyFlatSpec {

  "The sum of all points for all the cards in scratchcards1 file " should " be 13" in {
    assert(Scratchcards.getAllPointsFor(Source.fromResource("scratchcards1").getLines().toList) == 13)
  }

  "The sum of all points for all the cards in scratchcards2 file " should " be 18653" in {
    assert(Scratchcards.getAllPointsFor(Source.fromResource("scratchcards2").getLines().toList) == 18653L)
  }

  "The total number of scratchcards at the end of the process for the file scratchcards1 " should "be 30 " in {
    assert(Scratchcards.getTotalCardsFor(Source.fromResource("scratchcards1").getLines().toList) == 30L)
  }

  "The total number of scratchcards at the end of the process for the file scratchcards2 " should "be 5921508 " in {
    assert(Scratchcards.getTotalCardsFor(Source.fromResource("scratchcards2").getLines().toList) == 5921508L)
  }
}
