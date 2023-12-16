object PointOfIncidence {

  def totalNotesSum(rawNotes: List[String], smug: Boolean = false) = {
    def findMirrorLine(notes: List[(String, Int)]) = {
      val goDown = findMirrorsFromTop(notes,0)
      if goDown == 0 then
        val goUp = findMirrorsFromTop(notes.reverse, 0)
        if goUp == 0 then goUp else goUp - 1
      else
        goDown
    }
    def findMirrorsFromTop(notes: List[(String, Int)], acc: Int):Int = {
      def isAMirrorWithSmug(notes: Vector[(String, Int)]) = {
        val len = notes.size
        if len % 2 != 0 then -1
        else
          val lineWithErrors = (0 until len / 2).map(a => (notes(a)._1 == notes(len - 1 - a)._1, a)).filter(!_._1)
          if smug && lineWithErrors.size == 1 && notes(lineWithErrors.head._2)._1.zip(notes(len - 1 - lineWithErrors.head._2)._1).count(a => a._1 != a._2) == 1 then
            notes((len / 2) - 1)._2
          else if !smug && (0 until len / 2).forall(a => notes(a)._1 == notes(len - 1 - a)._1) then
            notes((len / 2) - 1)._2
          else
            -1
      }
      def isAMirror(notes:Vector[(String, Int)]) = {
        val len = notes.size
        if len % 2 != 0 then -1
        else if (0 until len / 2).forall(a => notes(a)._1 == notes(len - 1 - a)._1) then
          notes((len / 2) - 1)._2
        else -1
      }

      if notes.isEmpty then acc
      else if isAMirrorWithSmug(notes.toVector) != -1 then
        notes((notes.length / 2) - 1)._2 + 1
      else
        findMirrorsFromTop(notes.tail, 0)

    }
    def switchRwoByColums(notes: List[String]) = {
      notes.zipWithIndex.foldLeft(Map[Int, String]()) { (acc, row) =>
         row._1.split("").zipWithIndex.foldLeft(acc) { (a, element) =>
           if a.contains(element._2) then
             a.updated(element._2, a(element._2) + element._1)
           else
             a.updated(element._2, element._1)
         }
      }.toList.sortBy(_._1).map(_._2)
    }
    val notes = rawNotes.foldLeft(List[List[String]](List())) { (acc, next) =>
      if next == "" then
        List() :: acc
      else
        (next :: acc.head) :: acc.tail
    }.map(_.reverse).reverse
    notes.map{a => 100 * findMirrorLine(a.zipWithIndex)}.sum +
      notes.map(a => switchRwoByColums(a)).map{ a => findMirrorLine(a.zipWithIndex)}.sum

  }
}
