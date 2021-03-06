object SpecialMath {
  def main(args: Array[String]): Unit = {
    if(args.length != 1) {
      println("usage")
      System.exit(1)
    }
    try {
      println(extraSpecialMath(args(0).toInt))
      println(recursiveSpecialMath(args(0).toInt))
    } catch {
      case ex : Throwable =>
        println(s"error: ${ex.getMessage}")
        System.exit(1)
    }
  }

  /**
   * Original transcription from Python.  Blows up for large n.
   *
   * @param n
   * @return
   */
  def specialMath(n: Int): Int = {
    if(n < 0) throw new Exception("n must be >= 0")
    if(n < 2) n
    else n + specialMath(n - 1) + specialMath(n - 2)
  }

  /**
   * Non-recursive version, works great for n=90 (though for arbitrarily large n
   * Long won't be enough).
   *
   * Alternatives I considered to avoid the generated integer sequence:
   * - a recursive version, but that would basically do the same thing through the call stack
   * - a simple 'for' loop that updates the '(minus2, minus1)' tuple in a var; but i hate using vars
   * So I stuck with this.
   *
   * @param n
   * @return
   */
  def extraSpecialMath(n: Long): Long = {
    if(n < 0) throw new Exception("n must be >= 0")
    if(n < 2) n
    else (for(i <- 2L to n) yield i).foldLeft((0L, 1L)) {
      case ((minus2, minus1), i) => (minus1, i + minus1 + minus2)
    }._2
  }

  /**
   * Here's the tail-recursive version.  Should've gone ahead and done this, it's better.  Wasn't
   * thinking clearly.
   *
   * @param n
   * @return
   */
  def recursiveSpecialMath(n: Int): Long = {
    @scala.annotation.tailrec
    def recursivePart(prev: (Long, Long), i: Int, n: Int): Long = {
      if(i == n) i + prev._1 + prev._2
      else recursivePart((prev._2, i + prev._1 + prev._2), i + 1, n)
    }
    if(n < 0) throw new Exception("n must be >= 0")
    if(n < 2) n
    else recursivePart((0, 1), 2, n)
  }
}
