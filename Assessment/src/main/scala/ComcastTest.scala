import scala.annotation.tailrec

object ComcastTest {
  def main(args: Array[String]): Unit = {
    val test1 = List.empty[Char]
    println(s"$test1: ${findSegments(test1)}")
    println(s"$test1: ${findSegments2(test1)}\n")

    val test2 = List('a')
    println(s"$test2: ${findSegments(test2)}")
    println(s"$test2: ${findSegments2(test2)}\n")

    val test3 = List('a', 'a')
    println(s"$test3: ${findSegments(test3)}")
    println(s"$test3: ${findSegments2(test3)}\n")

    val test4 = List('a', 'b', 'a')
    println(s"$test4: ${findSegments(test4)}")
    println(s"$test4: ${findSegments2(test4)}\n")

    val test5 = List('a', 'b', 'c', 'd')
    println(s"$test5: ${findSegments(test5)}")
    println(s"$test5: ${findSegments2(test5)}\n")

    val test6 = List('e', 'a', 'b', 'a', 'b', 'c', 'd')
    println(s"$test6: ${findSegments(test6)}")
    println(s"$test6: ${findSegments2(test6)}\n")

    val test7 = List('a', 'b', 'a', 'c', 'd', 'c')
    println(s"$test7: ${findSegments(test7)}")
    println(s"$test7: ${findSegments2(test7)}\n")

    val test8 = List('e', 'a', 'b', 'a', 'c', 'd', 'c', 'e')
    println(s"$test8: ${findSegments(test8)}")
    println(s"$test8: ${findSegments2(test8)}")
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

  def findSegments2(input: List[Char]): List[Int] = {
    // More efficient implementation, replacing the zipWithIndex, groupBy, and
    // sortBy with a single tail-recursive traversal of the list to build the
    // groupings.  Assumes stable ordering of the values as new elements are
    // added.
    @tailrec
    def findGroups(input: List[Char], curIdx: Int, result: Map[Char, List[Int]]): List[(Int, Int)] = {
      if(input.isEmpty) result.values.map(v => (v.min, v.max)).toList
      else {
        val curChar = input.head
        val curResult = curChar -> (result.getOrElse(curChar, List.empty) :+ curIdx)
        findGroups(input.tail, curIdx + 1, result + curResult)
      }
    }

    findGroups(input, 0, Map.empty)
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
