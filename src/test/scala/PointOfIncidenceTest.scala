import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class PointOfIncidenceTest extends AnyFlatSpec {

  "The total sum of all notes of left column and row above multiplied by 100 of each mirror in mirrors1 file " should " be 405" in {
    assert(PointOfIncidence.totalNotesSum(Source.fromResource("mirrors1").getLines().toList) == 405L)
  }

  "The total sum of all notes of left column and row above multiplied by 100 of each mirror in mirrors3 file " should " be 400" in {
    assert(PointOfIncidence.totalNotesSum(Source.fromResource("mirrors3").getLines().toList) == 400L)
  }

  "The total sum of all notes of left column and row above multiplied by 100 of each mirror in mirrors2 file " should " be 28651" in {
    assert(PointOfIncidence.totalNotesSum(Source.fromResource("mirrors2").getLines().toList) == 28651L)
  }

  "The total sum of all notes of left column and row above multiplied by 100 of each mirror when take into account smug in mirrors1 file " should " be 400" in {
    assert(PointOfIncidence.totalNotesSum(Source.fromResource("mirrors1").getLines().toList, true) == 400L)
  }

  "The total sum of all notes of left column and row above multiplied by 100 of each mirror when take into account smug in mirrors2 file " should " be 25450" in {
    assert(PointOfIncidence.totalNotesSum(Source.fromResource("mirrors2").getLines().toList, true) == 25450L)
  }
}
