import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class TrebuchetTest extends AnyFlatSpec {
  "Adding all the calibration values (first and final digit on each line) for the file trebuchet1 " should "be 142 " in {
    assert(Trebuchet.calculateSumOfAllCalibrationValues(Source.fromResource("trebuchet1").getLines().toList) == 142)
  }

  "Adding all the calibration values (first and final digit on each line) for the file trebuchet2 " should "be 55971 " in {
    assert(Trebuchet.calculateSumOfAllCalibrationValues(Source.fromResource("trebuchet2").getLines().toList) == 55971)
  }

  "Adding all the calibration values even if they are in letters (first and final digit on each line) for the file trebuchet3 " should "be 281 " in {
    assert(Trebuchet.calculateSumOfAllCalibrationValuesAsSpelledAsWordsAndDigits(Source.fromResource("trebuchet3").getLines().toList) == 281)
  }

  "Adding all the calibration values even if they are in letters (first and final digit on each line) for the file trebuchet2 " should "be 54719 " in {
    assert(Trebuchet.calculateSumOfAllCalibrationValuesAsSpelledAsWordsAndDigits(Source.fromResource("trebuchet2").getLines().toList) == 54719)
  }
}
