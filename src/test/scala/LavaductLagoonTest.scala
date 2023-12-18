import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class LavaductLagoonTest extends AnyFlatSpec {

  "The total cubic meters that the lagoon can hold for the map described in lagoon1 file " should " be 62 " in {
    assert(LavaductLagoon.calculateTotalVolumeFor(Source.fromResource("lagoon1").getLines().toList) == 62L)
  }
  "The total cubic meters that the lagoon can hold for the map described in lagoon2 file " should " be 68115 " in {
    assert(LavaductLagoon.calculateTotalVolumeFor(Source.fromResource("lagoon2").getLines().toList) == 68115L)
  }
  "The total cubic meters that the lagoon can hold for the map described in lagoon1 file once fixed " should " be 952408144115 " in {
    assert(LavaductLagoon.calculateTotalVolumeFor2(Source.fromResource("lagoon1").getLines().toList) == 952408144115L)
  }
  "The total cubic meters that the lagoon can hold for the map described in lagoon2 file once fixed " should " be 71262565063800 " in {
    assert(LavaductLagoon.calculateTotalVolumeFor2(Source.fromResource("lagoon2").getLines().toList) == 71262565063800L)
  }
}
