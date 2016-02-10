/**
  * Created by ramp on 2/4/16.
  */
object SansaXor {
  def main(args: Array[String]) {
    doTheJob(io.Source.stdin.getLines) foreach println
  }

  def doTheJob(input: Iterator[String]): Iterator[String] = {
    input.next

    def iterator = new Iterator[String] {
      def hasNext = input.hasNext
      def next = {
        input.next
        xorArray(input.next.split(" ").map(_.toLong)).toString
      }
    }

    iterator
  }

  def xorArray(array: Seq[Long]) = {
    if (array.length % 2 == 0)
      0L
    else {
      val ret = array.indices by 2 map(array(_))
      ret.foldLeft(0L)(_^_)
    }
  }

  def elementOccurrenceCount(index: Int, length: Int) = {
    length % 2 match {
      case 0 => evenOccurrenceCount(index, length / 2)
      case 1 => oddOccurrenceCount(index, length / 2 + 1)
    }
  }

  def evenOccurrenceCount(k: Int, w: Int):Int = if (k<=w) 2*k*(w-k) + 2*k + k*(k-1) else evenOccurrenceCount(2*w-k+1, w)
  def oddOccurrenceCount(k: Int, w:Int):Int = if (k<=w) 2*k*(w-k) + k + k*(k-1) else oddOccurrenceCount(2*w-k, w)
}
