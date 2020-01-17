import java.util.{Base64 => JavaBase64}

object Base64 {
  /**
   * For the original implementation, I figured the test was to hand-code the
   * conversion to base64, so I did it that way.
   *
   * Just for fun, I've also added a version that uses the Java library base64
   * encoder, which is what I would do in "real life".
   * ETA: except after testing, I realized that this version doesn't work
   * correctly for odd-length input strings.  In that case, conversion to a
   * byte array results in an extra 'A' at the end of the encoded result.  D'oh!
   *
   * I also cleaned up the pattern matches a bit.
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {
    if(args.length != 1) {
      println("usage")
      System.exit(1)
    }
    try {
      val hex = args(0).map(Character.digit(_, 16))
      val b64 = hex.grouped(3).flatMap {
        case Seq(a, b, c) => Seq(a << 2 | b >> 2, (b & 3) << 4 | c)
        case Seq(a, b) => Seq(a << 2 | b >> 2, (b & 3) << 4)
        case Seq(a) => Seq(a << 2)
      }.toSeq
      println(encode(b64))
      val bytes = hex.grouped(2).map {
        case Seq(a, b) => (a << 4 | b).toByte
        case Seq(a) => (a << 4).toByte
      }.toArray
      println(JavaBase64.getEncoder.encodeToString(bytes))
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
