import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class TheFloorWillBeLavaTest extends AnyFlatSpec {
  "The number of tiles that end up energized in the contraption described in contraptions1 file " should " be 46 " in {
    assert(TheFloorWillBeLava.getTotalTilesEnergized(Source.fromResource("contraptions1").getLines().toList) == 46L)
  }
  "The number of tiles that end up energized in the contraption described in contraptions2 file " should " be 6902 " in {
    assert(TheFloorWillBeLava.getTotalTilesEnergized(Source.fromResource("contraptions2").getLines().toList) == 6902L)
  }
  "The number of tiles that end up energized the most in the contraption described in contraptions1 file " should " be 51 " in {
    assert(TheFloorWillBeLava.getMaxTotalTilesEnergized(Source.fromResource("contraptions1").getLines().toList) == 51L)
  }

  "The number of tiles that end up energized the most in the contraption described in contraptions2 file " should " be 7697 " in {
    assert(TheFloorWillBeLava.getMaxTotalTilesEnergized(Source.fromResource("contraptions2").getLines().toList) == 7697L)
  }
}
