import org.scalatest.{FunSpec, Matchers}
import TreeOfLife._

class TreeOfLifeTest extends FunSpec with Matchers {
  val onNode = Node(Terminal, Terminal, true)
  val offNode = Node(Terminal, Terminal, false)

  describe("parseTree") {
    it("should parse input string into tree") {
      parseTree(".") should be (offNode)
      parseTree("X") should be (onNode)
      parseTree("(. . .)") should be (Node(offNode, offNode, false))
      parseTree("(X X X)") should be (Node(onNode, onNode, true))
      parseTree("(X . (. X .))") should be (Node(onNode, Node(offNode, offNode, true), false))
      parseTree("((. X .) . X)") should be (Node(Node(offNode, offNode, true), ON, false))
      parseTree("(((((. X ((. . X) . X)) . (((X X (X . (X X X))) X X) . (((. X ((((X . X) X (. . .)) . (X X (. X (X X X)))) . (. . (X . ((X . X) . .))))) . X) X ((((X X .) . X) X .) X (X X (((((. . ((X X (X X .)) X (X . (((((X X .) . X) . (((. . (. X .)) X ((X . (X . ((((. X X) . X) . ((. . (. X ((. . X) X (. . (. X .))))) X X)) X .))) X (((((. X ((X X .) X .)) X X) X X) X X) X X))) . (((X X .) . X) X (X . ((. X ((. . (. X X)) . X)) X .))))) X X) X .)))) X (. X ((. X (X X .)) X .))) X (X X .)) . ((. . (. . .)) X (((. X X) X .) . (X X .)))) . (. . X))))))) . ((((. . (. X .)) . (X . .)) . .) X X)) . ((X . X) . (((X X .) . X) . ((((. . .) . .) . X) . ((X . X) X .))))) X (. X .))") should not be (Terminal)
    }
  }

  describe("parseRule") {
    it("should parse rule 7710") {
      val r = parseRule(7710)

      r(true, true, true, true) should be(false)
      r(false, true, true, true) should be(false)
      r(true, false, true, true) should be(false)
      r(false, false, true, true) should be(true)

      r(true, true, false, true) should be(true)
      r(false, true, false, true) should be(true)
      r(true, false, false, true) should be(true)
      r(false, false, false, true) should be(false)

      r(true, true, true, false) should be(false)
      r(false, true, true, false) should be(false)
      r(true, false, true, false) should be(false)
      r(false, false, true, false) should be(true)

      r(true, true, false, false) should be(true)
      r(false, true, false, false) should be(true)
      r(true, false, false, false) should be(true)
      r(false, false, false, false) should be(false)
    }

    it("should parse rule 42354") {
      val r = parseRule(42354)

      r(true, true, true, true) should be(true)
      r(false, true, true, true) should be(false)
      r(true, false, true, true) should be(true)
      r(false, false, true, true) should be(false)

      r(true, true, false, true) should be(false)
      r(false, true, false, true) should be(true)
      r(true, false, false, true) should be(false)
      r(false, false, false, true) should be(true)

      r(true, true, true, false) should be(false)
      r(false, true, true, false) should be(true)
      r(true, false, true, false) should be(true)
      r(false, false, true, false) should be(true)

      r(true, true, false, false) should be(false)
      r(false, true, false, false) should be(false)
      r(true, false, false, false) should be(true)
      r(false, false, false, false) should be(false)
    }
  }

  describe("drillInto") {
    it("should drill into tree") {
      val tree = parseTree("((. X .) . ((. X (. X .)) X .))")

      drillInto("", tree) should be(false)
      drillInto(">", tree) should be(true)
      drillInto("<>", tree) should be(false)
    }
  }

  describe("update Tree") {
    it("should update tree") {
      val src = parseTree("((. X (. . .)) . (X . (. X X)))")
      val fst = parseTree("((X . (. X .)) X (. X (X . X)))")
      val snd = parseTree("((. X (X . X)) . (X X (. X .)))")
      val trd = parseTree("((X . (. X .)) X (X . (X X X)))")
      val fth = parseTree("((. X (X . X)) . (. X (X . X)))")

      val rule = parseRule(42354)

      updateTree(src, rule) should be (fst)
      updateTree(fst, rule) should be (snd)
      updateTree(snd, rule) should be (trd)
      updateTree(trd, rule) should be (fth)
    }
  }

  describe("do the job") {
    it ("should do the job") {
      val input = """42354
                    |((. X (. . .)) . (X . (. X X)))
                    |6
                    |0 []
                    |2 [><]
                    |0 [><]
                    |0 [<>]
                    |1 [><]
                    |0 [<>]""".stripMargin.split("\n").toIterator

      doTheJob(input).mkString should be(".XX.XX")
    }

    it("should work on input00.txt resource") {
      import scala.io.Source

      val input = Source.fromURL(getClass.getResource("input00.txt"))
      val output = Source.fromURL(getClass.getResource("output00.txt"))

      doTheJob(input.getLines).toList should be (output.getLines.toList)

    }

    it("should work on input03.txt resource") {
      import scala.io.Source

      val input = Source.fromURL(getClass.getResource("input03.txt"))
      val output = Source.fromURL(getClass.getResource("output03.txt"))

      doTheJob(input.getLines).toList should be (output.getLines.toList)
    }
  }
}
