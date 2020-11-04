import scala.annotation.tailrec

object CourseraTest {
  def main(args: Array[String]): Unit = {
    val test1 = Map(
      List("A", "B", "C") -> 4,
      List("B", "C", "A") -> 3,
      List("C", "B", "A") -> 2)
    val test2 = Map(
      List("B", "A", "C") -> 4,
      List("A", "B") -> 3,
      List("A", "C") -> 3)

    printWinners("test1", test1)
    printWinners("test2", test2)
  }

  def printWinners(name: String, ballots: Map[List[String], Int]): Unit = {
    println(s"\n$name results:")
    println(s"The plurality winner is: ${pluralityWinner(ballots)}")
    println(s"The ranked winner is: ${rankedWinner(ballots)}")
  }

  def groupVotes(ballots: Map[List[String], Int]): Map[String, Int] =
    ballots.groupBy(_._1.head).map(grp => (grp._1, grp._2.values.sum))

  def pluralityWinner(ballots: Map[List[String], Int]): String = {
    groupVotes(ballots).maxBy(_._2)._1
  }

  @tailrec
  def rankedWinner(ballots: Map[List[String], Int]): String = {
    val maxVote = groupVotes(ballots).maxBy(_._2)
    if(maxVote._2 > ballots.values.sum / 2) maxVote._1
    else {
      val minVote = groupVotes(ballots).minBy(_._2)._1
      rankedWinner(
        ballots
          .filterNot(_._1 == List(minVote))
          .groupBy(_._1.filterNot(_ == minVote))
          .map(grp => (grp._1, grp._2.values.sum))
      )
    }
  }
}
