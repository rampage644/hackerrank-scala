import scala.annotation.tailrec
import scala.util.Sorting._
import scala.math._

object Solution {
  type Point = (Int, Int)

  def main(args: Array[String]): Unit = {
    val points = scala.io.Source.stdin.getLines().drop(1).map(_.split(" ")).map { case Array(x,y) => (x.toInt,y.toInt)} toList

    val hullPoints = hull (points)
    println(perimeter (hullPoints))
  }

  def hull(points: List[Point]):List[Point] = {
    val startPt = startPoint(points)
    val sortedPts = sortPointsCCW(startPt, points)

    scan3(startPt :: sortedPts)
  }

  def perimeter(pts: List[Point]): Double = {
    pts.zip(pts.drop(1) ++ pts.take(1)).map { case ((x0,y0),(x1,y1)) => sqrt(pow(x1-x0, 2) + pow(y1-y0, 2))}.sum
  }

  def scan3(pts: List[Point]): List[Point] = {
    def scanr(hull: List[Point], candidates: List[Point]): (List[Point], List[Point]) = {
      candidates match {
        case Nil => (hull, Nil)
        case (head :: tail) => {
          val (ref0, ref1) = (hull.init.last, hull.last)
          if (degree(ref0, ref1, head) >= 0)
            scanr(hull ++ List(head), tail)
          else
            scanr(hull.dropRight(1), head :: tail)
        }
      }
    }
    scanr(pts.take(2), pts.drop(2))._1
  }

  def scan(pts: List[Point]): List[Point] = {
    val stack = scala.collection.mutable.Stack[Point]()

    stack.push(pts.head, pts(1))

    var i = 2
    while (i < pts.length) {
      val top = stack.top

      if (top == pts.head) {
        stack.push(pts(i))
        i += 1
      }

      val sndTop = stack(1)
      if (degree(sndTop, top, pts(i)) > 0) {
        stack.push(pts(i))
        i += 1
      } else {
        stack.pop
      }

    }

    stack toList
  }

  def degree(pt0: Point, pt1: Point, pt2: Point): Int = {
    (pt1._1 - pt0._1) * (pt2._2 - pt0._2) - (pt2._1 - pt0._1) * (pt1._2 - pt0._2)
  }

  def sortPointsCCW(center: Point, pts: List[Point]): List[Point] = {
    def dist(pt1: Point, pt2: Point):Double = {
      val (x0, y0) = pt1
      val (x1, y1) = pt2
      sqrt(pow(x1-x0, 2) + pow(y1-y0, 2))
    }

    def cmp(pt1:Point, pt2:Point): Boolean = {
      degree(center, pt1, pt2) match {
        case 0 => dist(center, pt1) > dist(center, pt2)
        case x => x > 0
      }
    }
    stableSort(pts.filterNot(_ == center), cmp(_:Point, _:Point)).toList
  }

  def rightMost(pt0: Point, pt1: Point): Point = {
    val ((x0, y0), (x1, y1)) = (pt0, pt1)
    if (x0 == x1)
      if (y0 > y1) pt1 else pt0
    else
      if (x0 > x1) pt0 else pt1
  }

  def startPoint(pts: List[Point]) = {
    pts.fold(pts.head) {(pt0, pt1) => rightMost(pt0, pt1)}
  }

}



object Playground {

  def factorial(n: Int): Int = {
    @tailrec
    def factorialRec(n:Int, acc:Int): Int = {
      if (n == 1) acc else factorialRec(n-1, acc*n)
    }
    factorialRec(n, 1)
  }

  def balance(chars: List[Char]):Boolean = {
    @tailrec
    def balanceRec(chars: List[Char], acc:Int):Boolean = {
      if (chars.isEmpty && acc == 0) return true
      if (acc < 0) false else balanceRec(chars.tail, chars.head match {
        case '(' => acc+1
        case ')' => acc-1
        case _ => acc
      })
    }

    balanceRec(chars, 0)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) return 0
    if (coins.isEmpty) return 0
    1 + countChange(money, coins.tail)
  }

  @tailrec
  def nth[T](n: Int, list: List[T]): T = {
    if (list == Nil)
      throw new IndexOutOfBoundsException

    n match {
      case 0 => list.head
      case x if x > 0 => nth(n-1, list.tail)
    }

  }


}
