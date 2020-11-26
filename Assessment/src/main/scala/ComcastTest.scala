object ComcastTest {
  def main(args: Array[String]): Unit = {
    val test1 = List.empty[Char]
    println(findSegments(test1))

    val test2 = List('a')
    println(findSegments(test2))

    val test3 = List('a', 'a')
    println(findSegments(test3))

    val test4 = List('a', 'b', 'a')
    println(findSegments(test4))

    val test5 = List('a', 'b', 'c', 'd')
    println(findSegments(test5))

    val test6 = List('e', 'a', 'b', 'a', 'b', 'c', 'd')
    println(findSegments(test6))

    val test7 = List('a', 'b', 'a', 'c', 'd', 'c')
    println(findSegments(test7))

    val test8 = List('e', 'a', 'b', 'a', 'c', 'd', 'c', 'e')
    println(findSegments(test8))
  }

  def findSegments(input: List[Char]): List[Int] = {
    // This is the algorithm I was trying to get to, but couldn't quite get
    // my head around it on the spot:
    // - find first and last occurrence of each character
    // - sort by first occurrence
    // - coalesce overlapping segments
    // - take the 2nd element of remaining pairs
    // - result is the list of segment indexes
    input.zipWithIndex.groupBy(_._1)
      .map(v => (v._2.minBy(_._2)._2, v._2.maxBy(_._2)._2))
      .toSeq.sortBy(_._1)
      .foldLeft(List.empty[(Int, Int)]) {
        case (result, cur) if result.isEmpty => List(cur)
        case (result, cur) =>
          val prev = result.takeRight(1).head
          if(prev._2 > cur._2) result
          else if(prev._2 < cur._1) result :+ cur
          else result.take(result.length - 1) :+ (prev._1, cur._2)
      }
      .map(_._2)
  }
}
