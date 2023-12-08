import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class HauntedWastelandTest extends AnyFlatSpec {
  "The number of steps required to reach the point ZZZ with the map and instructions in wasteland1 file " should " be 6 " in {
    assert(HauntedWasteland.totalStepsToReachTheEndIn(Source.fromResource("wasteland1").getLines().toList) == 6L)
  }

  "The number of steps required to reach the point ZZZ with the map and instructions in wasteland2 file " should " be 24253 " in {
    assert(HauntedWasteland.totalStepsToReachTheEndIn(Source.fromResource("wasteland2").getLines().toList) == 24253L)
  }

  "The number of steps required to reach the point ..Z starting in all ..A with the map and instructions in wasteland2 file " should " be 12357789728873 " in {
    assert(HauntedWasteland.totalStepsToReachTheEndInForAllStartingPoints(Source.fromResource("wasteland2").getLines().toList) == 12357789728873L)
  }
}
