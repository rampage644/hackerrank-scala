import org.scalatest._
import TwosComplement._

class TwosComplementTest extends FunSuite with Matchers {

  test("sample input") {
    val input = """3
      |-2 0
      |-3 4
      |-1 4""".stripMargin.split("\n").toIterator

    doTheJob(input).toList should be ( "63" :: "99" :: "37" :: Nil)
  }


  test("ones sum count in numbers") {
    onesSumFromZeroPositive(1).sum should be (1)
    onesSumFromZeroPositive(2).sum should be (2)
    onesSumFromZeroPositive(4).sum should be (5)
    onesSumFromZeroPositive(16).sum should be (33)
    onesSumFromZeroPositive(17).sum should be (35)
    onesSumFromZeroPositive(3).sum should be (4)

    onesSumFromZeroNegative(-1).sum should be(31)
    onesSumFromZeroNegative(-2).sum should be(61)
    onesSumFromZeroNegative(-3).sum should be(91)
  }

  test("ones sum in range") {
    onesSumInRange(277, 415) should be (657)
    onesSumInRange(38179, 82060) should be (372508)
    onesSumInRange(-114, -8) should be (3058)

    onesSumInRange(-2, 0) should be(63)
    onesSumInRange(-3, 4) should be(99)
    onesSumInRange(-1, 4) should be(37)

    onesSumInRange(-2147483648, -2147483648) should be (1)
    onesSumInRange(-2147483648, 0) should be (35433480192L)
    onesSumInRange(-2147483648, -1) should be (35433480192L)


  }

  test("zero overlap") {
    val input = """1
                  |-1 1""".stripMargin.split("\n").toIterator

    doTheJob(input).toList should be ("33" :: Nil)
  }


  test("performance") {
    val input =
      """1
        |-2147483648 2147483646""".stripMargin.split("\n").toIterator


    doTheJob(input).toList should not be List.empty

  }

  test("output02.txt") {
    val input = scala.io.Source.fromURL(getClass getResource "twoscomplement_input02.txt") getLines
    val output = scala.io.Source.fromURL(getClass getResource "twoscomplement_output02.txt") getLines

    doTheJob(input).toList should be (output.toList)
  }

  test("output04.txt") {
    val input = scala.io.Source.fromURL(getClass getResource "twoscomplement_input04.txt") getLines
    val output = scala.io.Source.fromURL(getClass getResource "twoscomplement_output04.txt") getLines

    doTheJob(input).toList should be (output.toList)
  }
}
