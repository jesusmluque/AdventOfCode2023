import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class AlmanacSeedTest extends AnyFlatSpec {
  "The lowest location of all seeds for the almanac1 file " should " be 35 " in {
    assert(AlmanacSeed.getLowestLocation(Source.fromResource("almanac1").getLines().toList) == 35L)
  }

  "The lowest location of all seeds for the almanac2 file " should " be 551761867 " in {
    assert(AlmanacSeed.getLowestLocation(Source.fromResource("almanac2").getLines().toList) == 551761867L)
  }

  "The lowest location of all seeds for the almanac1 file taking into consideration that the seeds are ranges also " should " be 46 " in {
    assert(AlmanacSeed.getLowestLocationWhenSeedsAreRanges(Source.fromResource("almanac1").getLines().toList) == 46L)
  }

  "The lowest location of all seeds for the almanac2 file taking into consideration that the seeds are ranges also " should " be 57451709 " in {
    assert(AlmanacSeed.getLowestLocationWhenSeedsAreRanges(Source.fromResource("almanac2").getLines().toList) == 57451709L)
  }
}
