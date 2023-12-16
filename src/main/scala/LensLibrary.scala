object LensLibrary {

  def calculateFocusPowerIn(steps: List[String]) = {
    def replaceOrAddOrDelete(box: List[(String, Int)], lens: (String, Int), delete: Boolean = false) = {
      def replace(box: List[(String, Int)], lens: (String, Int)): List[(String, Int)] = {
        if box.isEmpty then
          if delete then List() else lens :: List()
        else if box.head._1 == lens._1 then
          if delete then box.tail else lens :: box.tail
        else
          box.head :: replace(box.tail, lens)
      }
      replace(box.reverse, lens).reverse
    }

    steps.head.split(",").foldLeft(Map[Int, List[(String, Int)]]()) { (acc, step) =>
      step match {
        case s"${label}=${focus}" => acc.get(hash(label)) match {
          case Some(box)  => acc.updated(hash(label), replaceOrAddOrDelete(box, (label, focus.toInt)))
          case None => acc.updated(hash(label), List((label, focus.toInt)))
        }
        case s"${label}-" => acc.get(hash(label)) match {
          case Some(box) => acc.updated(hash(label), replaceOrAddOrDelete(box, (label, 0), true))
          case None => acc
        }
      }
    }.foldLeft(0L) { (acc, box) =>
      box._2.reverse.zipWithIndex.map { lens =>
        (box._1 + 1).toLong * (lens._2 + 1) * lens._1._2.toLong
      }.sum + acc
    }
  }

  def sumAllHashesIn(steps: List[String]) = {
    steps.head.split(",").foldLeft(0L) { (acc, next) =>
      hash(next) + acc
    }
  }

  private def hash(label: String) = label.toCharArray.foldLeft(0) { (a, c) =>
    ((a + c.toInt) * 17) % 256
  }
}
