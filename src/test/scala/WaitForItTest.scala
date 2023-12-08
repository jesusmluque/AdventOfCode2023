import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class WaitForItTest extends AnyFlatSpec {
  "The product of all possibles ways to win the races with time 7,15,30 and distance 9,40,200 " should " be 288" in {
    val races = List((7,9), (15,40), (30,200))
    assert(WaitForIt.multiplyAllWaysToWinIn(races) == 288L)
  }

  "The product of all possibles ways to win the races with time 63,78,94,68 and distance 411,1274,2047,1035 " should " be 781200" in {
    val races = List((63, 411), (78, 1274), (94, 2047), (68, 1035))
    assert(WaitForIt.multiplyAllWaysToWinIn(races) == 781200L)
  }

  "The product of all possibles ways to win the races with time 71530 and distance 940200 " should " be 71503" in {
    val races = List((71530L, 940200L))
    assert(WaitForIt.multiplyAllWaysToWinIn2(races) == 71503)
  }

  "The product of all possibles ways to win the races with time 63789468L and distance 411127420471035L " should " be 49240091" in {
    val races = List((63789468L, 411127420471035L))
    assert(WaitForIt.multiplyAllWaysToWinIn2(races) == 49240091)
  }
}
