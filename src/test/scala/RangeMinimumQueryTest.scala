import org.scalatest.{FunSuite, Matchers}
import RangeMinimumQuery._

class RangeMinimumQueryTest extends FunSuite with Matchers{

  test("constructing tree from array") {
    val input2 = "10 20 30 40 11 22 33 44 15 5".split(" ") map (_.toInt)
    constructFromArray(input2) should not be(Terminal)
  }


  test("query for odd length arrays") {
    val input = "10 20 30 40 11 22 33 44 15 5".split(" ") map (_.toInt)
    val tree = constructFromArray(input)

    query(0, 5, tree, input.length - 1) should be(10)
    query(1, 2, tree, input.length - 1) should be(20)
    query(8, 9, tree, input.length - 1) should be(5)
    query(0, 9, tree, input.length - 1) should be(5)
    query(4, 6, tree, input.length - 1) should be(11)
  }
  test("query for even length arrays") {
    val input = "10 20 30 40 11 22 33 44 15 5 100".split(" ") map (_.toInt)
    val tree = constructFromArray(input)

    query(0, 5, tree, input.length-1) should be (10)
    query(1, 2, tree, input.length-1) should be (20)
    query(8, 9, tree, input.length-1) should be (5)
    query(0, 10, tree, input.length-1) should be (5)
    query(4, 6, tree, input.length-1) should be (11)
  }

  test("testable_main") {
    val in = """10 5
               |10 20 30 40 11 22 33 44 15 5
               |0 5
               |1 2
               |8 9
               |0 9
               |4 6""".stripMargin.split("\n").toIterator
    val out = """10
                |20
                |5
                |5
                |11""".stripMargin.split("\n")

    testable_main(in).toArray should be (out)
  }

  test("input05.txt") {
    val in = scala.io.Source.fromURL(getClass.getResource("input05.txt")).getLines()
    val out = scala.io.Source.fromURL(getClass.getResource("output05.txt")).getLines()

    testable_main(in).toArray should be (out.toArray)
  }

}
