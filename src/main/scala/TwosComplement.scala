import scala.io.Source.stdin

object TwosComplement {
  def main(args: Array[String]) {
    doTheJob(stdin.getLines()) foreach println
  }


  def doTheJob(input: Iterator[String]): Iterator[String] = {
    input.drop(1).map { x =>
      val Array(a, b) = x.split(" ")
      onesSumInRange(a.toLong, b.toLong) toString
    }
  }

  def onesSumFromZeroPositive(n: Long) = {
    val N = 31
    (1 to N).map(m => 1L << m).map(m => (n / m) * (m / 2) + ((n % m) - (m / 2 - 1) match {
      case n if n >= 0 => n
      case _ => 0
    }))
  }

  def onesSumFromZeroNegative(n: Long): Seq[Long] = {
    val N = 31
    val maxNumber = 1L << N
    val number = n & ~(1 << N)
    if (n == 0)
      return (1 to N).map(m => 0L)
    (1 to N).map(m => 1L << m).map(m => (maxNumber / m - number / m - 1) * (m / 2) + ((m - number % m) match {
      case n if n <= (m / 2) => n
      case _ => m / 2
    }))
  }

  def onesSumInRange(a: Long, b:Long) = {
    (a, b) match {
      case (x, y) if x >=0 && y >=0 => onesSumFromZeroPositive(y).zip(onesSumFromZeroPositive(x - 1)).map { case (a,b) => a-b }.sum
      case (x, y) if x < 0 && y < 0 => onesSumFromZeroNegative(x).zip(onesSumFromZeroNegative(y + 1)).map { case (a,b) => a-b }.sum + (y-x+1L)
      case (x, y) if x < 0 && y >=0 => onesSumFromZeroPositive(y).zip(onesSumFromZeroNegative(x)).map { case (a,b) => a+b }.sum + (-x)
    }
  }
}
