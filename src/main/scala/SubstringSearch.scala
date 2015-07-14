import scala.annotation.tailrec

object SubstringSearch {
  def main(args: Array[String]) {
    testable_main(scala.io.Source.stdin.getLines()) foreach println
  }

  def testable_main(it: Iterator[String]): Iterator[String] = new Iterator[String] {
    it.next()
    def hasNext = it.hasNext
    def next = if (kmpSearch(it.next(), it.next())) "YES" else "NO"
  }

  def buildTable(pattern:String): Array[Int] = {
    val res = new Array[Int](pattern.length)
    res(0) = -1

    var j = -1
    for (i <- 1 to res.length-1) {
      while (j >= 0 && pattern(j) != pattern(i-1)) j = res(j)
      j += 1
      res(i) = j
    }

    res
  }

  def kmpSearch(string:String, pattern:String): Boolean = {
    if (string.isEmpty || pattern.isEmpty)
      return false

    val table = buildTable (pattern)

    @tailrec
    def searchRec(i:Int,j:Int): Boolean = {
      if (j == pattern.length)
        return true
      if (i == string.length)
      return false


      if (string(i) == pattern(j))
        searchRec(i+1, j+1)
      else
        if (table(j) > -1)
          searchRec(i+j - table(j) - 1, table(j))
        else
          searchRec(i+1, 0)
    }

    searchRec(0, 0)
  }
}
