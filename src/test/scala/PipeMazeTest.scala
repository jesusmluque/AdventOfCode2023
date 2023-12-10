import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class PipeMazeTest extends AnyFlatSpec {
  "The number of steps to reach the farthest pipe from the start position in the maze into the file maze1 " should " be 8 " in {
    assert(MazePipe.getTotalStepsOfFarthestPipe(Source.fromResource("maze1").getLines().toVector) == 8)
  }

  "The number of steps to reach the farthest pipe from the start position in the maze into the file maze2 " should " be 7097 " in {
    assert(MazePipe.getTotalStepsOfFarthestPipe(Source.fromResource("maze2").getLines().toVector) == 7097)
  }

  "The number of tiles enclosed into the loop in the maze into the file maze3 " should " be 4 " in {
    assert(MazePipe.getTotalOfInternalTiles(Source.fromResource("maze3").getLines().toVector) == 4)
  }

  "The number of tiles enclosed into the loop in the maze into the file maze4 " should " be 8 " in {
    assert(MazePipe.getTotalOfInternalTiles(Source.fromResource("maze4").getLines().toVector) == 8)
  }

  "The number of tiles enclosed into the loop in the maze into the file maze5 " should " be 10 " in {
    assert(MazePipe.getTotalOfInternalTiles(Source.fromResource("maze5").getLines().toVector) == 10)
  }

  "The number of tiles enclosed into the loop in the maze into the file maze2 " should " be 355 " in {
    assert(MazePipe.getTotalOfInternalTiles(Source.fromResource("maze2").getLines().toVector) == 355)
  }
}
