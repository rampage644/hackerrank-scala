import scala.annotation.tailrec
import scala.io.Source.stdin

object PrefixCompression {
  def main (args: Array[String]) {
    val (input1 :: input2 :: Nil) = stdin.getLines().take(2).toList
    val (p,s1,s2) = prefixCompression(input1, input2)
    p :: s1 :: s2 :: Nil foreach (p => println(s"${p.size} $p"))
  }


  type Compressed = (String, String, String)
  def prefixCompression(s1:String, s2:String): Compressed = {

    @tailrec
    def findPrefix(s1: String, s2:String, previousPrefix:StringBuilder): Compressed = {
      (s1, s2) match {
        case ("", _) => (previousPrefix.toString(), s1, s2)
        case (_, "") => (previousPrefix.toString(), s1, s2)
        case (a, b) if a.head == b.head => findPrefix(s1.tail, s2.tail, previousPrefix.append(a.head))
        case _ =>  (previousPrefix.toString(), s1, s2)
      }
    }

    findPrefix(s1, s2, new StringBuilder)
  }
}
