import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source


class AplentyTest extends AnyFlatSpec {
  "The total sum of all rating numbers for all the parts accepted in the file description parts1 " should " be 19114 " in {
    assert(Aplenty.getTotalRatingNumbersSumFor(Source.fromResource("parts1").getLines().toList) == 19114L)
  }
  "The total sum of all rating numbers for all the parts accepted in the file description parts2 " should " be 425811 " in {
    assert(Aplenty.getTotalRatingNumbersSumFor(Source.fromResource("parts2").getLines().toList) == 425811L)
  }
  "All the combinations that are possible for the parts in the file description parts1 " should " be 167409079868000 " in {
    assert(Aplenty.getTotalCombinationsFor(Source.fromResource("parts1").getLines().toList) == 167409079868000L)
  }
}
