import scala.collection.mutable
import scala.util.control.Breaks._

object HotSprings {

  def totalUnfoldedSpringArrangements(rawSprings: List[String], times: Int):Long = {
    lazy val countArrangementsFor: ((Vector[String], List[Int])) => Long =  memoize { n =>
      val s = n._1
      val nums = n._2
      if nums.isEmpty then
        if !s.contains("#") then 1L else 0L
      else
        val size = nums.head
        var total = 0L
        breakable {
          for i <- s.indices do
            if (i + size <= s.length && !s.takeRight(s.length - i).take(size).contains(".")
              && (i == 0 || s(i - 1) != "#")
                && (i + size == s.length || s(i + size) != "#"))
              total = total + countArrangementsFor(s.takeRight(s.length - i - size - 1), nums.tail)

            if s(i) == "#" then
              break
        }
        total
    }
    rawSprings.map { row =>
      val List(s, c) = row.split(" ").toList
      val springs =  (s + (("?" + s) * (times - 1))).split("").toList
      val combinations = (c + (("," + c) * (times - 1))).split(",").map(_.toInt).toList
      countArrangementsFor((springs.toVector, combinations))
    }.sum
  }

  def totalArrangementsIn(rawSprings: List[String]) = {
    val springsCollection = rawSprings.map { row =>
      val List(s, c) = row.split(" ").toList
      val springs = s.split("").toList
      val combinations = c.split(",").map(_.toInt).toList
      (springs, combinations)
    }
    springsCollection.foldLeft(0) { (acc, spring) =>
      val pattern = "\\.*" + spring._2.map(n => s"#{${n}}").mkString("\\.+") + "\\.*"
      springArrangements(spring._1, List()).filter(_.size == spring._1.size).map(_.reverse)
        .count{ a =>
          pattern.r.matches(a.mkString(""))
        } + acc
    }
  }

  private def springArrangements(p: List[String], acc: List[List[String]]): List[List[String]] = {
    if (p.isEmpty)
      acc
    else
      val current = if acc.isEmpty then List[String]() else acc.head
      val next = p.head
      if (next == "?")
        val a = springArrangements(p.tail, ("#" :: current) :: acc)
        springArrangements(p.tail, ("." :: current) :: a)
      else
        val current = if acc.isEmpty then List[String]() else acc.head
        val next = p.head
        springArrangements(p.tail, (next :: current) :: acc)
  }

  private def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }
}
