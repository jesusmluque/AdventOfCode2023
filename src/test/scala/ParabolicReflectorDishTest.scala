import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class ParabolicReflectorDishTest extends AnyFlatSpec {
  "The total load of all rounded rocks after tilting the platform represented in platform1 file " should " be 136L " in {
    assert(ParabolicReflectorDish.totalLoadAfterNorthTilt(Source.fromResource("platform1").getLines().toVector) == 136L)
  }

  "The total load of all rounded rocks after tilting the platform represented in platform2 file " should " be 109424 " in {
    assert(ParabolicReflectorDish.totalLoadAfterNorthTilt(Source.fromResource("platform2").getLines().toVector) == 109424)
  }

  "The total load of all rounded rocks after 1000M cycles in the platform represented in platform1 file " should " be 64 " in {
    assert(ParabolicReflectorDish.totalLoadAfterCycles(Source.fromResource("platform1").getLines().toVector, 6L) == 64L)
  }

  "The total load of all rounded rocks after 1000M cycles in the platform represented in platform2 file " should " be 64 " in {
    assert(ParabolicReflectorDish.totalLoadAfterCycles(Source.fromResource("platform2").getLines().toVector, 109L) == 102509L)
  }
}
