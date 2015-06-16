import org.scalatest._
import Crossword1010._

class Crossword1010Test extends FlatSpec with Matchers {
  "Slots" should "be different" in {
    {
      VSlot(0, 0, 4).update("aaaa")
    } should not be {
      VSlot(0, 0, 4).update("aaab")
    }
  }

  "findSlots" should "find slot in +++++---++++" in {
    findSlots(List("++++----+++")) should be (List(HSlot(4,0,4)))
  }

  it should "find multiple slots" in {
    findSlots("+--+--+"::Nil) should be (HSlot(1,0,2)::HSlot(4,0,2)::Nil)
  }

  it should "find slots on edges" in {
    findSlots("--++--"::Nil) should be (HSlot(0,0,2)::HSlot(4,0,2)::Nil)
  }

  it should "skip single dashes" in {
    findSlots("+++-++++"::Nil) should be (List.empty)
  }

  it should "find 'vertical' slots" in {
    findSlots("+-+"::"+-+"::"+++"::Nil) should be (VSlot(1,0,2)::Nil)
  }

  it should "find all slots 1" in {
    val input =
    "+-++++++++" ::
    "+-++++++++" ::
    "+-++++++++" ::
    "+-----++++" ::
    "+-+++-++++" ::
    "+-+++-++++" ::
    "+++++-++++" ::
    "++------++" ::
    "+++++-++++" ::
    "+++++-++++" :: Nil

    findSlots(input) should be (HSlot(3,1,5) :: HSlot(7,2,6) :: VSlot(0,1,6) :: VSlot(3,5,7) :: Nil)
  }

  it should "find all slots 2" in {
    val input =
    "+-++++++++" ::
    "+-++++++++" ::
    "+-------++" ::
    "+-++++++++" ::
    "+-++++++++" ::
    "+------+++" ::
    "+-+++-++++" ::
    "+++++-++++" ::
    "+++++-++++" ::
    "++++++++++" :: Nil

    findSlots(input) should be (HSlot(2,1,7) :: HSlot(5,1,6) :: VSlot(0,1,7) :: VSlot(5,5,4) :: Nil)
  }

  "intersectsWith" should "detect intersection" in {
    VSlot(0,5,5).intersectsWith(HSlot(1,1,5)) should be (true)
    VSlot(1,1,3).intersectsWith(HSlot(1,1,3)) should be (true)
    HSlot(1,1,3).intersectsWith(VSlot(1,4,3)) should be (true)

    HSlot(0,0,6).intersectsWith(VSlot(0,0,3)) should be (true)
    HSlot(0,0,6).intersectsWith(VSlot(0,5,3)) should be (true)
  }

  it should "not detect intersection" in {
    HSlot(0,0,0).intersectsWith(HSlot(1,1,1)) should be (false)
    VSlot(0,0,0).intersectsWith(VSlot(1,1,1)) should be (false)

    HSlot(0,0,10).intersectsWith(VSlot(1,1,4)) should be (false)
    VSlot(0,0,10).intersectsWith(HSlot(2,2,4)) should be (false)
  }

  "Slot" should "update word when it fits its size" in {
    HSlot(0,0,5).update("abcde") should be (Some(HSlot(0,0,5,"abcde")))
    VSlot(4,3,1).update("a") should be (Some(VSlot(4,3,1,"a")))
  }

  it should "not update when it doesn't" in {
    HSlot(0,0,6).update("abcde") should be (None)
    VSlot(4,3,0).update("a") should be (None)
  }

  it should "update" in {
    val hslot = HSlot(0,0,6)
    val vslot1 = VSlot(0,0,3)
    val vslot2 = VSlot(0,5,3)
    val all = hslot :: vslot1 :: vslot2 :: Nil

    hslot.update("abcdef", all) should be (Some(HSlot(0,0,6,"abcdef")))
    vslot1.update("abc", all) should be (Some(VSlot(0,0,3,"abc")))
    vslot2.update("fgh", all) should be (Some(VSlot(0,5,3,"fgh")))

    hslot.update("ayyyyf", all) should be (Some(HSlot(0,0,6,"ayyyyf")))
  }

  it should "not update on collisions" in {
    val hslot = HSlot(0,0,6).update("abcdef").get
    var vslot1:Slot = VSlot(0,0,3)
    var vslot2:Slot = VSlot(0,5,3)
    var all = hslot :: vslot1 :: vslot2 :: Nil

    vslot1.update("bcd", all) should be (None)
    vslot2.update("abd", all) should be (None)

    vslot1 = vslot1.update("abd", all).get
    vslot2 = vslot2.update("fgh", all).get

    all = hslot :: vslot1 :: vslot2 :: Nil
    hslot.update("abcdez", all) should be (None)
  }

  "intersectsAt" should "find correct intersection coordinate" in {
    val slot = HSlot(1,1,5)
    slot.intersectsAt(VSlot(1,1,4)) should be ((1,1))
    slot.intersectsAt(VSlot(0,4,3)) should be ((1,4))
    slot.intersectsAt(VSlot(0,2,3)) should be ((1,2))
  }

  "symbolAt" should "return symbol" in {
    val slot = HSlot(0,0,5).update("abcde").get

    slot.symbolAt(0,0) should be ('a')
    slot.symbolAt(0,3) should be ('d')

    val vslot = VSlot(2,2,4).update("abcd").get
    vslot.symbolAt(2,2) should be ('a')
    vslot.symbolAt(4,2) should be ('c')

    slot.symbolAt(0,0,"ccccc") should be ('c')
  }

  "generateChildren" should "generate all children" in {
    val inputSet: Set[Slot] = Set(VSlot(0,0,2), VSlot(0,0,3))
    generateChildren((inputSet, List("aa", "bbb")), Set.empty) should be (
      (Set(VSlot(0,0,2,"aa"), VSlot(0,0,3)), List("bbb")) ::
      (Set(VSlot(0,0,2), VSlot(0,0,3,"bbb")), List("aa")) :: Nil
    )
  }

  "generateChildren" should "generate children for one" in {
    generateChildren((Set(VSlot(0,0,1)), List("a")), Set.empty) should be (
      (Set(VSlot(0,0,1, "a")), List.empty) :: Nil
    )
  }

  "dfs" should "find correct solution with one goal state" in {
    val rootState:Crossword1010.State = (Set(VSlot(0,0,3)), List("aaa"))
    val goalState = (Set(VSlot(0,0,3,"aaa")), List.empty)
    dfs(rootState, Set.empty) should be (Some(goalState))
  }

  "dfs" should "find correct solution with colisions" in {
    val rootState:Crossword1010.State = (Set(HSlot(0,0,3), VSlot(0,1,3)), List("abc", "bba"))
    val goalState = (Set(HSlot(0,0,3,"abc"), VSlot(0,1,3,"bba")), List.empty)
    dfs(rootState, Set.empty) should be (Some(goalState))
  }

  "solveCrossword" should "solve 1" in {
    val input = """+-++++++++
+-++++++++
+-++++++++
+-----++++
+-+++-++++
+-+++-++++
+++++-++++
++------++
+++++-++++
+++++-++++
LONDON;DELHI;ICELAND;ANKARA""".split("\n").toList
    val solution = solveCrossword(input.take(10), input(10).split(";").toList)
    solution should be (Some((Set(VSlot(0,1,6,"LONDON"), HSlot(3,1,5,"DELHI"), VSlot(3,5,7,"ICELAND"), HSlot(7,2,6,"ANKARA")),List.empty)))
  }

  "solveCrossword" should "solve 2" in {
    val input = """+-++++++++
+-++++++++
+-------++
+-++++++++
+-++++++++
+------+++
+-+++-++++
+++++-++++
+++++-++++
++++++++++
AGRA;NORWAY;ENGLAND;GWALIOR""".split("\n").toList
    val solution = solveCrossword(input.take(10), input(10).split(";").toList)
    solution should be (Some((Set(VSlot(0,1,7,"ENGLAND"), HSlot(2,1,7,"GWALIOR"), VSlot(5,5,4,"AGRA"), HSlot(5,1,6,"NORWAY")),List.empty)))
  }

  "printSolution" should "print 1" in {
    val solution:Crossword1010.State = (Set(VSlot(0,1,7,"ENGLAND"), HSlot(2,1,7,"GWALIOR"), VSlot(5,5,4,"AGRA"), HSlot(5,1,6,"NORWAY")), List.empty[String])
    printSolution(solution) should be ("""+E++++++++
+N++++++++
+GWALIOR++
+L++++++++
+A++++++++
+NORWAY+++
+D+++G++++
+++++R++++
+++++A++++
++++++++++""")

  }

}
