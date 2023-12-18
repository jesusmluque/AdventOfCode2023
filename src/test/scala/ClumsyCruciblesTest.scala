import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class ClumsyCruciblesTest extends AnyFlatSpec {
  "The least heat loss path for the crucibles in the map described on the crucibles1 file " should " be 102 " in {
    assert(ClumsyCrucibles.minHeatLoss(Source.fromResource("crucibles1").getLines().toVector) == 102L)
  }
  "The least heat loss path for the crucibles in the map described on the crucibles2 file " should " be 665 " in {
    assert(ClumsyCrucibles.minHeatLoss(Source.fromResource("crucibles2").getLines().toVector) == 665L)
  }
  "The least heat loss path for the crucibles taking a maximum of 10 steps in the same direction in the map described on the crucibles1 file " should " be 94 " in {
    assert(ClumsyCrucibles.minHeatLoss(Source.fromResource("crucibles1").getLines().toVector, 10, 4) == 94L)
  }
  "The least heat loss path for the crucibles taking a maximum of 10 steps in the same direction in the map described on the crucibles2 file " should " be 809 " in {
    assert(ClumsyCrucibles.minHeatLoss(Source.fromResource("crucibles2").getLines().toVector, 10, 4) == 809L)
  }
  "The least heat loss path for the crucibles taking a maximum of 10 steps in the same direction in the map described on the crucibles3 file " should " be 94 " in {
    assert(ClumsyCrucibles.minHeatLoss(Source.fromResource("crucibles3").getLines().toVector, 10, 4) == 71L)
  }
}
