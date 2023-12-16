def replaceOrAddOrDelete(box: List[(String, Int)], lens: (String, Int), delete: Boolean = false) = {
  def replace(box: List[(String, Int)], lens: (String, Int)): List[(String, Int)] = {
    if box.isEmpty then
      if delete then
        List()
      else
        lens :: List()
    else if box.head._1 == lens._1 then
      if delete then
        box.tail
      else
        lens :: box.tail
    else
      box.head :: replace(box.tail, lens)
  }

  replace(box.reverse, lens).reverse
}

val a = List(("hl",1),("gp",1),("xl",1),("lf",1),("mn",1))
replaceOrAddOrDelete(a, ("gp", 0), true)