import org.scalatest.{FunSpec, Matchers}
import SuperQueens._

class SuperQueensTest extends FunSpec with Matchers {
  describe("solution") {
    it("should find solutions for 10 board") {
      solveNSuperQueens(10) should be (4)
    }

  }

  describe("attacks") {
    it("should detect attack with same row") {
      val (p0, p1) = ((1,1), (1,2))
      attacks(p0, p1) should be (true)
    }

    it("should detect diagonal attack with negative slope") {
      val (p0, p1) = ((1,1), (2,2))
      attacks(p0, p1) should be (true)
    }

    it("should detect diagonal attack with positive slope") {
      val (p0, p1) = ((2,2), (1,3))
      attacks(p0, p1) should be (true)
    }

    it("should detect L shape attack") {
      attacks((1,1), (3,2)) should be (true)
      attacks((1,1), (2,3)) should be (true)
    }

    it("should not detect attacks from safe pos") {
      attacks((1,1), (4,2)) should be (false)
      attacks((1,1), (4,3)) should be (false)
      attacks((1,1), (2,4)) should be (false)
      attacks((1,1), (3,4)) should be (false)
    }
  }
}
