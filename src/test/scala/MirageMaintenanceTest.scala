import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class MirageMaintenanceTest extends AnyFlatSpec {
  "The sum of all the extrapolated values from each history value on the set of histories in the file histories1 " should " be 114 " in {
    assert(MirageMaintenance.sumAllExtrapolatedLastValuesFrom(Source.fromResource("histories1").getLines().toList) == 114)
  }

  "The sum of all the extrapolated values from each history value on the set of histories in the file histories2 " should " be 1731106378 " in {
    assert(MirageMaintenance.sumAllExtrapolatedLastValuesFrom(Source.fromResource("histories2").getLines().toList) == 1731106378L)
  }

  "The sum of all the extrapolated first values from each history value on the set of histories in the file histories1 " should " be 2 " in {
    assert(MirageMaintenance.sumAllExtrapolatedFirstValuesFrom(Source.fromResource("histories1").getLines().toList) == 2L)
  }

  "The sum of all the extrapolated first values from each history value on the set of histories in the file histories2 " should " be 1087 " in {
    assert(MirageMaintenance.sumAllExtrapolatedFirstValuesFrom(Source.fromResource("histories2").getLines().toList) == 1087L)
  }
}
