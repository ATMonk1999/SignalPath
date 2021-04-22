import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object MyImplicits {
  implicit class MyStringOps(self: String) {
    /**
     * Split a string around a separator with optional maximum number of results,
     * applying an operation to each resulting element.
     *
     * @param sep the separator character
     * @param maxResultLen maximum allowed result length
     * @param op operation to apply to results of the split
     * @tparam R type of results of the operation
     * @return either a failure message or the result of splitting the string and applying the operation
     */
    def splitWithOp[R](sep: Char, maxResultLen: Option[Int] = None)(op: String => R): Either[String, Seq[R]] = {
      @tailrec
      def doSplit(result: Seq[R], len: Int, str: String): Either[String, Seq[R]] = {
        @tailrec
        def nextPart(acc: String, str: String): (String, String) = {
          if(str.isEmpty) (acc, str)
          else if(str.head == sep) (acc, str.tail)
          else nextPart(acc + str.head, str.tail)
        }

        if(str.isEmpty) Right(result)
        else if(maxResultLen.exists(len > _)) Left(s"max result length ${maxResultLen.get} exceeded")
        else {
          val (part, remain) = nextPart(acc = "", str)
          Try(op(part)) match {
            case Failure(ex) => Left(s"operation failed on input '$part': ${ex.getMessage}")
            case Success(r) => doSplit(result :+ r, len + 1, remain)
          }
        }
      }

      doSplit(Seq.empty[R], len = 0, self)
    }
  }
}
