import scala.annotation.tailrec

object AlmanacSeed {

  def getLowestLocationWhenSeedsAreRanges2(rawAlmanac: List[String]) = {
    def mapRanges(range: List[Long], ranges: List[List[Long]], mapping: List[List[Long]]): List[List[Long]] = {
      val minValue = range.head
      val maxValue = range.head + range(1)
      if (ranges.isEmpty)
        range :: mapping
      else if (minValue >= ranges.head(1) && maxValue <= ranges.head(1) + ranges.head(2))
        List(minValue - ranges.head(1) + ranges.head.head, maxValue - ranges.head(1) + ranges.head.head) :: mapping
      else if (minValue >= ranges.head(1) && minValue <= (ranges.head(1) + ranges.head(2)) && maxValue > ranges.head(1) + ranges.head(2))
        mapRanges(List(ranges.head(1) + ranges.head(2), maxValue), ranges.tail, List(minValue - ranges.head(1) + ranges.head.head, ranges.head(1) + ranges.head.head) :: mapping)
      else if (minValue < ranges.head(1) && maxValue >= ranges.head(1) && maxValue <= ranges.head(1) + ranges.head(2))
        mapRanges(List(minValue, ranges.head(1)), ranges.tail, List(ranges.head.head, ranges.head.head + range(1) - ranges.head(1) + minValue) :: mapping)
      else if (minValue < ranges.head(1) && maxValue > ranges.head(1) + ranges.head(2))
        val mappingInt = mapRanges(List(minValue, ranges.head(1)), ranges.tail, List(ranges.head.head, ranges.head.head + ranges.head(2)) :: mapping)
        mapRanges(List(ranges.head(1) + ranges.head(2), maxValue), ranges.tail, mappingInt)
      else
        mapRanges(range, ranges.tail, mapping)
    }
    val rawParts = parseAlmanac(rawAlmanac)
    val seedsRanges = rawParts("seed").head.grouped(2).toList
    seedsRanges.flatMap { pair =>
      val soil = mapRanges(pair, rawParts("soil"), List())
      val fertilizer = soil.flatMap(mapRanges(_, rawParts("fertilizer"), List()))
      val water = fertilizer.flatMap(mapRanges(_, rawParts("water"), List()))
      val light = water.flatMap(mapRanges(_, rawParts("light"), List()))
      val temperature = light.flatMap(mapRanges(_, rawParts("temperature"), List()))
      val humidity = temperature.flatMap(mapRanges(_, rawParts("humidity"), List()))
      val location = humidity.flatMap(mapRanges(_, rawParts("location"), List()))
      location
    }.minBy(_.head).head
  }
  def getLowestLocationWhenSeedsAreRanges(rawAlmanac: List[String]):Long = {
    val rawParts = parseAlmanac(rawAlmanac)
    val seedsRanges = rawParts("seed").head.grouped(2).toList
    seedsRanges.flatMap { pair =>
      (pair.head to pair.head + pair(1)).map{ seed =>
        val soil = mapValues(seed, rawParts("soil"))
        val fertilizer = mapValues(soil, rawParts("fertilizer"))
        val water = mapValues(fertilizer, rawParts("water"))
        val light = mapValues(water, rawParts("light"))
        val temperature = mapValues(light, rawParts("temperature"))
        val humidity = mapValues(temperature, rawParts("humidity"))
        mapValues(humidity, rawParts("location"))
      }
    }.min
  }
  def getLowestLocation(rawAlmanac: List[String]) = {
    val rawParts = parseAlmanac(rawAlmanac)
    val seeds = rawParts("seed").head
    seeds.map { seed =>
      val soil = mapValues(seed, rawParts("soil"))
      val fertilizer = mapValues(soil, rawParts("fertilizer"))
      val water = mapValues(fertilizer, rawParts("water"))
      val light = mapValues(water, rawParts("light"))
      val temperature = mapValues(light, rawParts("temperature"))
      val humidity = mapValues(temperature, rawParts("humidity"))
      mapValues(humidity, rawParts("location"))
    }.min
  }
  @tailrec
  private def mapValues(value: Long, ranges: List[List[Long]]):Long = {
    if (ranges.isEmpty)
      value
    else
      val range = ranges.head
      if ( value >= range(1) && value < range(1) + range(2))
        value - range(1) + range.head
      else
        mapValues(value, ranges.tail)
  }
  private def parseAlmanac(rawAlmanac: List[String]) = {
    def parseNumbersLine(nums: String) = nums.split(" ").map(_.toLong).toList
    rawAlmanac.foldLeft(Map[String, List[List[Long]]](), "") { (acc, next) =>
      next match {
        case "" => acc
        case s"seeds: ${seeds}" => (acc._1.updated("seed", List(parseNumbersLine(seeds))), acc._2)
        case s"seed-to-soil map:" => (acc._1, "soil")
        case s"soil-to-fertilizer map:" => (acc._1, "fertilizer")
        case s"fertilizer-to-water map:" => (acc._1, "water")
        case s"water-to-light map:" => (acc._1, "light")
        case s"light-to-temperature map:" => (acc._1, "temperature")
        case s"temperature-to-humidity map:" => (acc._1, "humidity")
        case s"humidity-to-location map:" => (acc._1, "location")
        case numbers: String => acc._2 match {
          case "soil" if acc._1.contains("soil") => (acc._1.updated("soil", parseNumbersLine(numbers) :: acc._1("soil")), acc._2)
          case "soil" => (acc._1.updated("soil", List(parseNumbersLine(numbers))), acc._2)
          case "fertilizer" if acc._1.contains("fertilizer") => (acc._1.updated("fertilizer", parseNumbersLine(numbers) :: acc._1("fertilizer")), acc._2)
          case "fertilizer" => (acc._1.updated("fertilizer", List(parseNumbersLine(numbers))), acc._2)
          case "water" if acc._1.contains("water") => (acc._1.updated("water", parseNumbersLine(numbers) :: acc._1("water")), acc._2)
          case "water" => (acc._1.updated("water", List(parseNumbersLine(numbers))), acc._2)
          case "light" if acc._1.contains("light") => (acc._1.updated("light", parseNumbersLine(numbers) :: acc._1("light")), acc._2)
          case "light" => (acc._1.updated("light", List(parseNumbersLine(numbers))), acc._2)
          case "temperature" if acc._1.contains("temperature") => (acc._1.updated("temperature", parseNumbersLine(numbers) :: acc._1("temperature")), acc._2)
          case "temperature" => (acc._1.updated("temperature", List(parseNumbersLine(numbers))), acc._2)
          case "humidity" if acc._1.contains("humidity") => (acc._1.updated("humidity", parseNumbersLine(numbers) :: acc._1("humidity")), acc._2)
          case "humidity" => (acc._1.updated("humidity", List(parseNumbersLine(numbers))), acc._2)
          case "location" if acc._1.contains("location") => (acc._1.updated("location", parseNumbersLine(numbers) :: acc._1("location")), acc._2)
          case "location" => (acc._1.updated("location", List(parseNumbersLine(numbers))), acc._2)
        }
      }
    }._1
  }
}
