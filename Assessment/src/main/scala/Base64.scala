object Base64 {
  def main(args: Array[String]): Unit = {
    if(args.length != 1) {
      println("usage")
      System.exit(1)
    }
    try {
      val b64 = args(0).map(Character.digit(_, 16)).grouped(3).flatMap {
        case grp if grp.length == 1 => Seq(grp(0) << 2)
        case grp if grp.length == 2 => Seq(grp(0) << 2 | grp(1) >> 2, (grp(1) & 3) << 4)
        case grp => Seq(grp(0) << 2 | grp(1) >> 2, (grp(1) & 3) << 4 | grp(2))
      }.toSeq
      println(s"${encode(b64)}")
    } catch {
      case ex : Throwable =>
        println(s"error: ${ex.getMessage}")
        System.exit(1)
    }
  }

  val codes: String = ((for(ch <- 'A' to 'Z') yield ch) ++
    (for(ch <- 'a' to 'z') yield ch) ++
    (for(ch <- '0' to '9') yield ch)).mkString("", "", "+/")

  def encode(b64: Seq[Int]): String = {
    val padding = if(b64.length % 4 == 0) 0 else 4 - b64.length % 4
    b64.map(codes.charAt).padTo(b64.length + padding, '=').mkString("")
  }
}
