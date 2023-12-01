object Trebuchet:

  def calculateSumOfAllCalibrationValuesAsSpelledAsWordsAndDigits(entries: List[String]) = {
    def findDigitsIn(l: String, patternsToMatch: List[String], f:String => String) = {
      patternsToMatch.foldLeft(Map[Int, String]()) { (acc, next) =>
        next.r.findAllMatchIn(l).map(_.start).foldLeft(acc) { (a, n) =>
          a.updated(n, f(next))
        }
      }
    }
    val digitsSpelledOutWithLetters = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    val digits = List("1","2","3","4","5","6","7","8","9")
    val translation = digitsSpelledOutWithLetters.zip(digits).toMap
    entries.map { line =>
      val onlyDigitsFound = findDigitsIn(line, digits, a => a)
      val spelledOutDigitsFound = findDigitsIn(line, digitsSpelledOutWithLetters, translation)
      val all = onlyDigitsFound ++ spelledOutDigitsFound
      (all.minBy(_._1)._2 + all.maxBy(_._1)._2).toLong
    }.sum
  }
  def calculateSumOfAllCalibrationValues(entries: List[String]) = {
    entries.foldLeft(0L) {(acc, line) =>
      val digits = line.split("").filter { nextChar =>
        nextChar.toLongOption match {
          case Some(d) => true
          case _ => false
        }
      }
      (digits.head + digits.last).toLong + acc
    }
  }
