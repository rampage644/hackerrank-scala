import org.scalatest._
import Playground._

class playground_test extends FlatSpec {
  "balance" should "return false with ':-)'" in {
    assert(balance(":-)".toList) == false)
  }

  it should "return false with '())('" in {
    assert(balance("())(".toList) == false)
  }

  it should "return true with '(if (zero? x) max (/ 1 x))'" in {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  it should "return true with ' told him (that it’s not (yet) done). (But he wasn’t listening)'" in {
    assert(balance(" told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
  }

  "countChange" should "return 6 for 17 and (1,5,10,25,50,100)" in {
    assert(countChange(17, List(1,5,10,25,50,100)) == 6)
  }

  it should "return 6 for 15 and (1,5,10,25,50,100)" in {
    assert(countChange(15, List(1, 5, 10, 25, 50, 100)) == 6)
  }

  it should "return 4 for 11 and (1,5,10)" in {
    assert(countChange(11, List(1,5,10)) == 4)
  }

  it should "return 3 for 4 and (1,2)" in {
    assert(countChange(11, List(1,2)) == 3)
  }

  "nth" should "return first element" in {
    assert(nth(0, 1::2::3::Nil) == 1)
  }

  it should "throw on errorneous indices" in {
    intercept[IndexOutOfBoundsException] {
      nth(-1, Nil)
    }

    intercept[IndexOutOfBoundsException] {
      nth(5, Nil)
    }
  }

  it should "return correct nth element" in {
    assert(nth(3, 1::2::3::4::Nil) == 4)
  }
}

class SolutionTest extends FunSuite with Matchers {

  test("start point") {
    Solution.startPoint(List((0, 0), (1, 0), (0, 1), (1, 1))) should be(1, 0)
  }

  test("rightMost") {
    Solution.rightMost((0,0), (1,1)) should be (1,1)
    Solution.rightMost((0,0), (0,1)) should be (0,0)
    Solution.rightMost((0,0), (1,0)) should be (1,0)
  }

  test("degree") {
    assert(Solution.degree((0, 0), (1, 1), (1, 0)) < 0)
    assert(Solution.degree((0, 0), (1, 1), (0, 1)) > 0)

    assert(Solution.degree((0, 0), (1, 1), (2, 2)) == 0)
  }

  test("sort CCW") {
    Solution.sortPointsCCW((0,0), List((0,1), (1,0), (1,1))) should be (List((1,0), (1,1), (0,1)))
    Solution.sortPointsCCW((0,0), List((1,1), (2,2))) should be (List((1, 1), (2, 2)))
    Solution.sortPointsCCW((5,3), List((1,1), (2,5), (5,3), (3,3), (3,2), (2,2))) should be (List((2,5), (3,3), (2,2), (1,1), (3,2)))

  }

  test("hull") {
    val pts = List((1,1), (2,5), (3,3), (5,3), (3,2), (2,2))

    Solution.hull(pts) should be (
      List((3,2), (1,1), (2,5), (5,3))
    )

  }

  test("pre100") {
    val input = List((11, 749), (23, 748), (77, 285), (53, 551), (92, 351), (17,  26), (47, 203), (50, 991), (26, 634))
    val hull = List((92, 351), (50,991), (11, 749), (17, 26))

    Solution.hull(input) should be (List((26,3), (26,26), (17,17), (17,11), (17,6)))
  }

  test("100") {
    val input =   List((113,201), (911,749), (839,217), (293,144), (290,848), (350,150), (143,995), (311,262), (923,748), (599,691), (128,790), (611,723), (881,577), (446,988), (209,589), (977,285), (512,813), (875,788), (566,674), (788,872), (320,738), (743,446), (227,271), (617,470), (761,859), (860,918), (866,868), (746,640), (167,39), (824,768), (593,184), (248,831), (197,232), (224,13), (677,131), (554,31), (35,572), (485,367), (422,828), (689,657), (314,954), (863,753), (806,315), (953,551), (992,351), (212,436), (917,26), (719,948), (707,606), (947,203), (119,798), (791,919), (260,394), (950,991), (59,164), (5,341), (92,191), (338,504), (383,695), (476,888), (602,133), (68,80), (818,277), (713,617), (827,971), (533,671), (455,300), (29,682), (605,71), (8,555), (32,449), (545,843), (215,526), (857,237), (926,634), (539,889), (335,656), (443,431), (269,402), (770,190), (680,978), (494,344), (242,763), (317,560), (803,73), (20,604), (785,154), (380,96), (536,669), (395,251), (236,977), (437,818), (389,412), (356,435), (23,500), (725,597), (587,481), (368,630), (776,791), (560,249))
    val hull = List((977,285), (917,26), (224,13), (68,80), (5,341), (8,555), (29,682), (143,995), (950,991), (992,351))
    Solution.hull(input) should be (hull.reverse)
      assert(Solution.perimeter(hull) - 3589.18 < 0.01)
  }

  test("grid") {
    val input = List((26,  3), (17, 11), (17,  6), (26, 24), (26, 26), (17, 17), (26, 15))
    Solution.startPoint(input) should be (26,3)
    Solution.sortPointsCCW((26,3), input) should be (List((26,26), (26,24), (26,15), (17,17), (17,11), (17,6)))
//    val input = List((44, 44), (26, 45), (17, 41), (26,  3), (44, 15), (26, 38), (26, 37),  (44, 28), (44, 45), (44,  3), (17, 46), (44, 38), (44, 37), (26, 47), (17, 11), (17,  6), (26, 24), (26, 26), (44, 47), (17, 17), (26, 44), (44, 24), (44, 26), (26, 15))
    Solution.hull(input) should be ()
  }

  test("perimeter") {
    val pts = List((1,1), (2,5), (3,3), (5,3), (3,2), (2,2))
    assert(Solution.perimeter(Solution.hull(pts)) - 12.2 < 0.001)
  }
}
