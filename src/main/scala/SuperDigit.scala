import scala.annotation.tailrec
import scala.io.Source.stdin
import java.io.EOFException

object SuperDigit {
  def main(args: Array[String]) {
    print(superDigitWithCount(stdin))
  }

  def superDigitWithCount(input: Iterator[Char]) : Int = {
    val baseDigit = superDigit(input.takeWhile(!_.isWhitespace))
    superDigit((input.dropWhile(_.isWhitespace).mkString("").toInt * baseDigit).toString.toIterator)
  }

  def superDigit(input: Iterator[Char]): Int = {

    @tailrec
    def superDigitRecursive(input: Iterator[Char], prev:Char, currentSum: Int):Int = {
      val digit = prev.asDigit
      val sum = if (currentSum + digit >= 10) (currentSum+digit) % 10 + 1 else currentSum+digit

      if (!input.isEmpty) superDigitRecursive(input, input.next(), sum) else sum
    }

    superDigitRecursive(input, '0', 0)
  }
}
