import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class CosmicExpansionTest extends AnyFlatSpec {

  "The sum of all shortest paths between all the galaxy pairs after expand the universe in the universe1 file" should " be 374 " in {
    assert(CosmicExpansion.sumAllShortestPath(Source.fromResource("universe1").getLines().toVector) == 374L)
  }

  "The sum of all shortest paths between all the galaxy pairs after expand the universe in the universe2 file" should " be 9521550 " in {
    assert(CosmicExpansion.sumAllShortestPath(Source.fromResource("universe2").getLines().toVector) == 9521550L)
  }

  "The sum of all shortest paths between all the galaxy pairs after expand 10 times the universe in the universe1 file" should " be 1030 " in {
    assert(CosmicExpansion.sumAllShortestPath(Source.fromResource("universe1").getLines().toVector, 10L) == 1030L)
  }

  "The sum of all shortest paths between all the galaxy pairs after expand 100 times the universe in the universe1 file" should " be 1030 " in {
    assert(CosmicExpansion.sumAllShortestPath(Source.fromResource("universe1").getLines().toVector, 100L) == 8410L)
  }

  "The sum of all shortest paths between all the galaxy pairs after expand 1000000 times the universe in the universe2 file" should " be 298932923702 " in {
    assert(CosmicExpansion.sumAllShortestPath(Source.fromResource("universe2").getLines().toVector, 1000000L) == 298932923702L)
  }
}
