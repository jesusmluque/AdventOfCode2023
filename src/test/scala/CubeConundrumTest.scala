import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class CubeConundrumTest extends AnyFlatSpec {

  "The sum of all the game ids that are possible when there are 12 red 13 green and 14 blue cubes for the file conundrum1 " should " be 8 " in {
    assert(CunundrumCube.sumAllValidGamesIdsIn(Source.fromResource("cunundrum1").getLines().toList) == 8L)
  }

  "The sum of all the game ids that are possible when there are 12 red 13 green and 14 blue cubes for the file conundrum2 " should " be 2369 " in {
    assert(CunundrumCube.sumAllValidGamesIdsIn(Source.fromResource("cunundrum2").getLines().toList) == 2369L)
  }

  "The sum of the power of each game for the minimum set of cubes possible in the file conundrum1 " should " be 2286 " in {
    assert(CunundrumCube.sumOfPowerForEachGameIn(Source.fromResource("cunundrum1").getLines().toList) == 2286L)
  }

  "The sum of the power of each game for the minimum set of cubes possible in the file conundrum2 " should " be 66363 " in {
    assert(CunundrumCube.sumOfPowerForEachGameIn(Source.fromResource("cunundrum2").getLines().toList) == 66363L)
  }
}
