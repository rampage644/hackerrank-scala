import org.scalatest.{FunSpec, Matchers}
import SuperDigit._


class SuperDigitTest extends FunSpec with Matchers {
  describe("superDigit") {
    it ("should return digit itself if 1-digit") {
      0 to 9 foreach (i => superDigit(s"$i".toIterator) should be (i))
    }

    it ("should calculate super digit with simple cases") {
      superDigit("99".toIterator) should be (9)
      superDigit("54".toIterator) should be (9)
      superDigit("123".toIterator) should be (6)
      superDigit("12256".toIterator) should be (7)
      superDigit("1834681".toIterator) should be (4)
      superDigit("148148148".toIterator) should be (3)
    }
  }

  describe("superDigit with count separated by space") {
    it ("should find correct super digit") {
      superDigitWithCount("148 3".toIterator) should be (superDigit("148148148".toIterator))
    }
  }

}
