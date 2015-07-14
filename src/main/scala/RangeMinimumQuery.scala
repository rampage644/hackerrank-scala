import scala.io.Source.stdin

object RangeMinimumQuery {

  abstract class Tree
  case class Node(l:Tree, r:Tree,v:Int) extends Tree {
    def this(v:Int) = this(Terminal, Terminal, v)
  }
  case object Terminal extends Tree

  implicit def tree2Int(node: Tree) = node match {
    case Node(_,_,v) => v
    case Terminal => Integer.MAX_VALUE
  }

  def constructFromArray(input: Array[Int]): Tree = {
    def construct0(s:Int, e:Int, input:Array[Int]): Tree = {
      if (s == e)
        return new Node(input(s))

      val m = s + (e-s)/2
      val (l, r) = (construct0(s,m,input), construct0(m+1,e,input))
      val value = scala.math.min(l, r)

      Node(l,r,value)
    }

    construct0(0, input.length-1, input)
  }

  def query(qs:Int, qe:Int, input:Tree, length: Int):Int = {

    def query0(qs:Int, qe:Int, s:Int, e:Int, node:Tree):Int = {
      node match {
        case Node(l,r,v) if qs <= s && e <= qe => v
        case _ if qs > e || qe < s => Terminal
        case Node(l,r,v) => {
          val m = s+(e-s)/2
          scala.math.min(query0(qs,qe, s, m, l), query0(qs,qe, m+1,e, r))
        }
      }
    }

    query0(qs,qe, 0, length, input)
  }

  def testable_main(it:Iterator[String]): Iterator[String] = {
    val length = it.next().split(" ")(0).toInt
    val tree = constructFromArray(it.next().split(" ").map(_.toInt))
    it map (_.split(" ") map (_.toInt)) map { case Array(qs,qe) => query(qs, qe, tree, length-1).toString }
  }

  def main(args: Array[String]) {
    testable_main(stdin getLines) foreach println
  }
}
