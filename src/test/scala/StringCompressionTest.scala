import org.scalatest._

class StringCompressionTest extends FunSuite with Matchers {
  test("abcd") {
    Solution2.compress("abcd") should be ("abcd")
  }

  test("abcaaabbb") {
    Solution2.compress("abcaaabbb") should be ("abca3b3")
  }

  test("aaabaaaaccaaaaba") {
    Solution2.compress("aaabaaaaccaaaaba") should be ("a3ba4c2a4ba")
  }

  test("empty string") {
    Solution2.compress("") should be ("")
  }

  test("single char") {
    Solution2.compress("a") should be ("a")
  }

  test("double char") {
    Solution2.compress("aa") should be ("a2")
  }

  test("double digit") {
    Solution2.compress("aaaaaaaaaaaa") should be ("a12")
  }

  test("input09") {
    import scala.io.Source.fromURL
    val input = getClass getResource "/input09.txt"
    val output = getClass getResource "/output09.txt"

    val compressed = fromURL(input).getLines().mkString
    Solution2.compress(compressed) should be (fromURL(output).getLines().mkString)
  }

  test("input11") {
    import scala.io.Source.fromURL
    val input = getClass getResource "/input11.txt"
    val output = getClass getResource "/output11.txt"

    val compressed = fromURL(input).getLines().mkString
    Solution2.compress(compressed) should be (fromURL(output).getLines().mkString)
  }
}
