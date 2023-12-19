object Aplenty {

  def getTotalCombinationsFor(rawData: List[String]) = {
    def traverseWorkflowFor(wf: Map[String, Array[(Option[(String, String, String)], String)]], next:String, acc: List[List[(String, (Long,Long))]]):List[List[(String, (Long,Long))]] = {
      val inst = wf(next)
      inst.foldLeft(acc) { (a, i) =>
        i match
          case (Some((nameVar, oper, value)), newNext) if oper == "<" =>
            if newNext == "R" then
              acc
            else if newNext == "A" then
              ((nameVar, (0, value.toLong)) :: acc.head) :: acc
            else
              traverseWorkflowFor(wf, newNext, ((nameVar, (0, value.toLong)) :: acc.head) :: acc)
          case (Some((nameVar, oper, value)), newNext) if oper == ">" =>
            if newNext == "R" then
              acc
            else if newNext == "A" then
              ((nameVar, (value.toLong, 4000L)) :: acc.head) :: acc
            else
              traverseWorkflowFor(wf, newNext, ((nameVar, (value.toLong, 4000L)) :: acc.head) :: acc)
      }
      acc
    }
    val List(workflowsRaw, _) = rawData.splitAt(rawData.indexOf("")).toList
    val instructions = parseWorkfows(rawData)

    0L
  }

  def getTotalRatingNumbersSumFor(rawData: List[String]) = {
    def calculateWorkflowFor(part: Map[String, Long], next: String, workflow: Map[String, Array[(Option[(String, String, String)], String)]]):Boolean = {
      def nextWorkflow(wf: Array[(Option[(String, String, String)], String)], acc: String):String = {
        if wf.isEmpty then
          acc
        else
          wf.head match
            case (Some((nameVar, oper, value)), newNext) if oper == "<" =>
              if part(nameVar) < value.toLong then
                newNext
              else
                nextWorkflow(wf.tail, acc)
            case (Some((nameVar, oper, value)), newNext) if oper == ">" =>
              if part(nameVar) > value.toLong then
                newNext
              else
                nextWorkflow(wf.tail, acc)
            case (None, newNext) => nextWorkflow(wf.tail, newNext)
      }

      if next == "A" then
        true
      else if next == "R"then
        false
      else
        val wf = workflow(next)
        calculateWorkflowFor(part, nextWorkflow(wf, "R"), workflow)

    }
    val List(workflowsRaw, partsRaw) = rawData.splitAt(rawData.indexOf("")).toList
    val instructions = parseWorkfows(rawData)
    val parts:List[Map[String, Long]]  = partsRaw.tail.map{ next =>
      val s"{${ps}}" = next
      ps.split(",").foldLeft(Map[String, Long]()) { (acc, n) => n match
        case s"x=${x}" => acc.updated("x", x.toLong)
        case s"a=${x}" => acc.updated("a", x.toLong)
        case s"s=${x}" => acc.updated("s", x.toLong)
        case s"m=${x}" => acc.updated("m", x.toLong)
      }
    }
    parts.filter(p => calculateWorkflowFor(p, "in", instructions)).foldLeft(0L) {(acc, next) =>
      next.values.sum + acc
    }
  }

  private def parseWorkfows(workflowsRaw: List[String]) = {
    val instructions = workflowsRaw.filter(_ != "").foldLeft(Map[String, Array[(Option[(String, String, String)], String)]]()) { (acc, next) =>
      next match
        case s"${name}{${instructions}}" => acc.updated(name, instructions.split(",").map {
          case s"{${a}<${value}:${next}" => (Some((a, "<", value)), next)
          case s"{${a}>${value}:${next}" => (Some((a, ">", value)), next)
          case s"${a}<${value}:${next}" => (Some((a, "<", value)), next)
          case s"${a}>${value}:${next}" => (Some((a, ">", value)), next)
          case s"$next" => (None, next)
        })
    }
    instructions
  }
}
