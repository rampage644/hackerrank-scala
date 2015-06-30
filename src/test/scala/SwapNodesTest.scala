import org.scalatest.{FunSpec, Matchers}
import SwapNodes._

class SwapNodesTest extends FunSpec with Matchers {
  describe("readTree") {
    it ("should read simple tree") {
      val input = """2 3
-1 -1
-1 -1""".split("\n")

      val root = readTree(input)
      root should be (Node(Node(Terminal, Terminal, 2), Node(Terminal, Terminal, 3), 1))
      depth(root) should be (2)
    }

    it("should read more complicated tree") {
      val input =
        """2 3
-1 4
-1 5
-1 -1
-1 -1""".split("\n")

      val root = readTree(input)

      root should be (
        Node(
          Node(Terminal, Node(Terminal, Terminal, 4), 2),
          Node(Terminal, Node(Terminal, Terminal, 5), 3),
          1))

      depth(root) should be (3)
    }

    it("should read 1024-depth tree") {
      val d = 1024
      val input = 2 to d map (row => s"-1 $row")

      val root = readTree(input :+ ("-1 -1"))
      depth(root) should be (d)
    }
  }

  describe("swap at") {

    it ("should not swap one-node tree") {
      val tree = Node(Terminal, Terminal, 1)
      swappedAt(tree, 1) should be (tree)

      swappedAt(tree, 6) should be (tree)
    }

    it ("should swap simple tree") {
      val tree = Node(Node(Terminal, Terminal, 2), Node(Terminal, Terminal, 3), 1)
      swappedAt(tree, 1) should be (Node(Node(Terminal, Terminal, 3), Node(Terminal, Terminal, 2), 1))
    }

    it ("should not swap simple tree") {
      val tree = Node(Node(Terminal, Terminal, 2), Node(Terminal, Terminal, 3), 1)
      swappedAt(tree, 2) should be (tree)
    }

    it ("should swap all nodes when depth-mod is 1") {
      val tree = readTree("""2 3
4 5
6 7
-1 -1
-1 -1
-1 -1
-1 -1""".split("\n"))

      val resultTree = readTree(
        """3 2
5 4
7 6
-1 -1
-1 -1
-1 -1
-1 -1""".split("\n"))
      swappedAt(tree, 1) should be (resultTree)
    }

  }

  describe("print tree") {
    it ("should print one root tree") {
      printTree(Node(Terminal, Terminal, 1)) should be ("1")
    }

    it ("should print simple tree") {
      val tree = Node(Node(Terminal, Terminal, 2), Node(Terminal, Terminal, 3), 1)
      printTree(tree) should be ("2 1 3")
    }

    it ("should print more complex tree") {
      val tree = readTree("""2 3
4 5
6 7
-1 -1
-1 -1
-1 -1
-1 -1""".split("\n"))

      printTree(tree) should be ("4 2 5 1 6 3 7")
    }
  }

  describe("solve") {
    it("should solve simple case") {
      val input = """3
                    |2 3
                    |-1 -1
                    |-1 -1
                    |2
                    |1
                    |1""".stripMargin.split("\n")

      solve(input.toIterator).toList should be ("3 1 2" :: "2 1 3" :: Nil)
    }

    it ("should solve case #1") {
      val input = """5
                    |2 3
                    |-1 4
                    |-1 5
                    |-1 -1
                    |-1 -1
                    |1
                    |2""".stripMargin.split("\n")

      solve(input.toIterator).toList should be ("4 2 1 5 3" :: Nil)
    }

    it ("should solve case #2") {
      val input = """11
                    |2 3
                    |4 -1
                    |5 -1
                    |6 -1
                    |7 8
                    |-1 9
                    |-1 -1
                    |10 11
                    |-1 -1
                    |-1 -1
                    |-1 -1
                    |2
                    |2
                    |4
                    |""".stripMargin.split("\n")

      solve(input.toIterator).toList should be ("2 9 6 4 1 3 7 5 11 8 10" :: "2 6 9 4 1 3 7 5 10 8 11" :: Nil)
    }
  }
}
