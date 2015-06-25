object SuperQueens {
  type Position = (Int, Int)

  def attacks(p1: Position, p2:Position) = {
    (p1, p2) match {
      case ((r1,c1), (r2,c2)) if r1 == r2 || c1 == c2 => true
      case ((r1,c1), (r2,c2)) if (r2 - r1).abs == (c2 - c1).abs => true
      case ((r1,c1), (r2,c2)) if (r2 - r1).abs == 1 && (c2 - c1).abs == 2 => true
      case ((r1,c1), (r2,c2)) if (r2 - r1).abs == 2 && (c2 - c1).abs == 1 => true
      case _ => false
    }
  }

  def solutionsForBoard(c:Int, N:Int, queens: List[Position]):Int = {
    if (c > N)
      return 1
    1 to N map ((_, c)) filter (pos => queens forall (queen => !attacks(pos, queen)) ) map (pos => solutionsForBoard(c+1, N, queens :+ (pos._1, c))) sum
  }

  def solveNSuperQueens(n: Int): Int = {
    solutionsForBoard(1, n, Nil)
  }


  def main(args: Array[String]) {
    println(solveNSuperQueens(readInt))
  }
}