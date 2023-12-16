import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class HotSpringsTest extends AnyFlatSpec {
  "The total number of different arrangements of operational and broker springs in the springs1 file" should " be 21 " in {
    assert(HotSprings.totalUnfoldedSpringArrangements(Source.fromResource("springs1").getLines().toList, 1) == 21L)
  }

  "The total number of different arrangements of operational and broker springs in the springs2 file" should " be 8022 " in {
    assert(HotSprings.totalUnfoldedSpringArrangements(Source.fromResource("springs2").getLines().toList, 1) == 8022L)
  }

  "The total number of different arrangements of operational and broker springs once those are unfolded in the springs1 file" should " be 525152 " in {
    assert(HotSprings.totalUnfoldedSpringArrangements(Source.fromResource("springs1").getLines().toList, 5) == 525152L)
  }

  "The total number of different arrangements of operational and broker springs once those are unfolded in the springs2 file" should " be 4968620679637 " in {
    assert(HotSprings.totalUnfoldedSpringArrangements(Source.fromResource("springs2").getLines().toList, 5) == 4968620679637L)
  }

}
