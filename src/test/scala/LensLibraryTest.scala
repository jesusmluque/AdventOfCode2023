import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class LensLibraryTest extends AnyFlatSpec {
  "The sum of all hashes for each step in the file sequences1 " should " be 1320 " in {
    assert(LensLibrary.sumAllHashesIn(Source.fromResource("sequences1").getLines().toList) == 1320L)
  }

  "The sum of all hashes for each step in the file sequences2 " should " be 517551 " in {
    assert(LensLibrary.sumAllHashesIn(Source.fromResource("sequences2").getLines().toList) == 517551L)
  }

  "The total focusing power of all lens in all boxes in the file sequences1 " should " be 145 " in {
    assert(LensLibrary.calculateFocusPowerIn(Source.fromResource("sequences1").getLines().toList) == 145L)
  }

  "The total focusing power of all lens in all boxes in the file sequences2 " should " be 286097 " in {
    assert(LensLibrary.calculateFocusPowerIn(Source.fromResource("sequences2").getLines().toList) == 286097L)
  }
}
