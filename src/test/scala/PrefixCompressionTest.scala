import org.scalatest.{FunSpec, Matchers, BeforeAndAfter}
import scala.util.Random
import PrefixCompression._

class PrefixCompressionTest extends FunSpec with Matchers {

  describe("prefixCompression"){
    it("should not compress string with no common prefix") {
      prefixCompression("absdasd", "usad,baksjd") should be (("", "absdasd", "usad,baksjd"))
    }

    it ("should compress when they have one") {
      prefixCompression("asdabsdasd", "asdusad,baksjd") should be (("asd", "absdasd", "usad,baksjd"))
    }

    it ("should not compress when either string is empty") {
      prefixCompression("", "aaaa") should be (("", "", "aaaa"))
      prefixCompression("aaaa", "") should be (("", "aaaa", ""))
    }

    it ("should work on long strings") {
      val length = 100000
      val input1 = Random.nextString(length)
      prefixCompression(input1, input1) should be (input1, "", "")
    }
  }
}
