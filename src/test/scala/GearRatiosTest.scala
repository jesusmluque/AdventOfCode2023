import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class GearRatiosTest extends AnyFlatSpec {

  "The sum of all the part numbers in the gearRatios1 file " should " be 4361 " in {
    assert(GearRatios.sumAllParts(Source.fromResource("gearRatios1").getLines().toVector) == 4361L)
  }

  "The sum of all the part numbers in the gearRatios3 file " should " be 4981 " in {
    assert(GearRatios.sumAllParts(Source.fromResource("gearRatios3").getLines().toVector) == 4981L)
  }

  "The sum of all the part numbers in the gearRatios2 file " should " be 527446 " in {
    assert(GearRatios.sumAllParts(Source.fromResource("gearRatios2").getLines().toVector) == 527446L)
  }

  "The sum of all the gears in the gearRatios1 file " should " be 467835 " in {
    assert(GearRatios.sumAllGears(Source.fromResource("gearRatios1").getLines().toVector) == 467835L)
  }
  "The sum of all the gears in the gearRatios2 file " should " be 73201705 " in {
    assert(GearRatios.sumAllGears(Source.fromResource("gearRatios2").getLines().toVector) == 73201705L)
  }
}