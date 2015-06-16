import scala.io.Source.stdin
import scala.util.{Success, Try}

abstract class Slot(r:Int,c:Int,size:Int,currentWord: String = "") {

  def intersectsWith(other: Slot) = {
    (this, other) match {
      case (VSlot(r1,c1,s1,_), HSlot(r2,c2,s2,_)) if c2 <= c1 && c2 + s2 >= c1 && r1 <= r2 && r1+s1 >= r2 => true
      case (HSlot(r2,c2,s2,_), VSlot(r1,c1,s1,_)) if c2 <= c1 && c2 + s2 >= c1 && r1 <= r2 && r1+s1 >= r2 => true
      case _ => false
    }
  }

  def intersectsAt(other: Slot) = {
    (this, other) match {
      case (VSlot(r1,c1,s1,_), HSlot(r2,c2,s2,_)) => (r2, c1)
      case (HSlot(r2,c2,s2,_), VSlot(r1,c1,s1,_)) => (r2, c1)
    }
  }

  def symbolAt(r: Int, c: Int, text:String = word) = {
    this match {
      case VSlot(r0,_,_,_) => text(r-r0)
      case HSlot(_,c0,_,_) => text(c-c0)
    }
  }

  def update(text: String, other:Iterable[Slot] = Nil):Option[Slot] = {
    if (text.length != size)
      return None

    val intersections = other.filter(_.intersectsWith(this))
    if (intersections.exists( other => {
      val (r,c) = other.intersectsAt(this)
      (other.word, text) match {
        case ("", _) => false
        case (_, "") => false
        case (_, _) => other.symbolAt(r,c) != symbolAt(r,c,text)
      }

    } ))
      return None


    Some(create(text))
  }

  def word = currentWord
  def create(text:String): Slot
}

case class HSlot(r:Int,c:Int,size:Int, currentWord:String="") extends Slot(r,c,size,currentWord) {
  override def toString = s"H:$r,$c,$size,$currentWord"
  override def create(text:String):Slot = HSlot(r,c,size,text)
}

case class VSlot(r:Int,c:Int,size:Int,currentWord:String="") extends Slot(r,c,size,currentWord) {
  override def toString = s"V:$r,$c,$size,$currentWord"
  override def create(text:String):Slot = VSlot(r,c,size,text)
}



object Crossword1010 {
  type State = (Set[Slot], List[String])

  def main (args: Array[String]) {
    val input =  stdin.getLines().toList
    solveCrossword(input.take(10), input(10).split(';').toList) match {
      case Some(solution) => print(printSolution (solution))
      case None => print("No solution")
    }
  }

  def printSolution(solution: State): String = {
    def symbolAt(r:Int, c:Int): Char = solution._1.collect {
      case slot@VSlot(r0,c0,s,_) if c0 == c && r0 <= r && r0+s > r => slot.symbolAt(r,c)
      case slot@HSlot(r0,c0,s,_) if r0 == r && c0 <= c && c0+s > c => slot.symbolAt(r,c)
    } headOption match {
      case Some(symbol) => symbol
      case None => '+'
    }

    val (height, width) = (10, 10)
    0 until height map (r => 0 until width map (c => symbolAt(r,c)) mkString) mkString "\n"
  }

  def solveCrossword(crossword:List[String], words:List[String]):Option[State] = {
    val slots = findSlots(crossword)
    dfs((slots.toSet, words), Set.empty)
  }

  def isGoal(node: State): Boolean = {
    val (slots, words) = node
    slots.forall( !_.word.isEmpty) && words.isEmpty
  }

  def generateChildren(node: State, visited: Set[State]): List[State] = {
    // get still empty slots
    val (slots, words) = node
    val (free, occupied) = slots.partition(_.word.isEmpty)
    if (free.isEmpty)
      return Nil

    val states = for {
      word <- words
      slot <- free
      slotCandidate <- slot.update(word, slots)
    } yield (occupied + slotCandidate ++ (free - slot), words.filter(_ != word))

    states.filterNot(visited.contains)
  }

  def dfs(node: State, visited: Set[State]): Option[State] = {
    if (isGoal(node))
      return Some(node)


     val children = generateChildren(node, visited + node)
     if (children.isEmpty)
       return None

     children.flatMap(child => dfs(child, visited + node)) match {
       case child :: rest => Some(child)
       case Nil => None
     }

  }

  def findSlots(text: List[String]): List[Slot] = {
    def swapXY(slot: HSlot) = {
      VSlot(slot.c, slot.r, slot.size)
    }

    def slotStarts(current: Char, prev:Char):Boolean = {
      (prev, current) match {
        case ('+', '-') => true
        case _ => false
      }
    }

    def slotEnds(current: Char, prev:Char):Boolean = {
      (prev, current) match {
        case ('-', '+') => true
        case _ => false
      }
    }


    def slotrecursive(text: Seq[Char], prev:Char, startC: Int, r: Int, c:Int) : List[HSlot] = {
      val current = if (text.isEmpty) '+' else text.head

      text match {
        case Nil if slotEnds(current, prev) && (c-startC>1) => HSlot(r, startC, c-startC) :: Nil
        case Nil => Nil
        case a if slotStarts(current, prev) => slotrecursive(a.tail, current, c, r, c+1)
        case a if slotEnds(current, prev) && (c-startC>1) => HSlot(r, startC, c-startC) +: slotrecursive(a.tail, current, 0, r, c+1)
        case a => slotrecursive(text.tail, current, startC, r, c+1)
      }

    }

    text.zipWithIndex.flatMap { case (text, r) => slotrecursive(text, '0', 0, r, 0)} ++
      text.map(_.toSeq).transpose.zipWithIndex.flatMap { case (text, r) => slotrecursive(text, '0', 0, r, 0)}.map( swapXY )
  }

}
