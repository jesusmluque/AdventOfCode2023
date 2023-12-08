import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source


class CamelCardsTest extends AnyFlatSpec {

  "The total winning value for the list of hands in the input file hands1 " should " be 6440 " in {
    assert(CamelCards.calculateTotalWinningFor(Source.fromResource("hands1").getLines().toList) == 6440L)
  }

  "The total winning value for the list of hands in the input file hands2 " should " be 254024898 " in {
    assert(CamelCards.calculateTotalWinningFor(Source.fromResource("hands2").getLines().toList) == 254024898L)
  }

  "The total winning value taking into account Js for the list of hands in the input file hands1 " should " be 5905 " in {
    assert(CamelCards.calculateTotalWinningWithJsFor(Source.fromResource("hands1").getLines().toList) == 5905)
  }

  "The total winning value taking into account Js for the list of hands in the input file hands2 " should " be 254115617 " in {
    assert(CamelCards.calculateTotalWinningWithJsFor(Source.fromResource("hands2").getLines().toList) == 254115617L)
  }
}
