import scala.io.StdIn
import scala.annotation.tailrec

object Solution2 {
  def main(args: Array[String]) = {
    print(compress (StdIn.readLine))
  }

  def compress(text: String): String = {

    @tailrec
    def compressr(text: String, pos: Int, sym:Char, count:Int, output: Seq[Char]): Seq[Char] = {
      val current = if (text.length == pos) '0' else text(pos)

      current match {
        case '0' if count == 1 => sym +: output
        case '0' => count.toString.toSeq.reverse ++: sym +: output
        case s if s == sym => compressr(text, pos + 1, s, count + 1, output)
        case s if count == 1 => compressr(text, pos + 1, s, 1, sym +: output)
        case s => compressr(text, pos + 1, s, 1, count.toString.toSeq.reverse ++: sym +: output)
      }
    }

    if (!text.isEmpty)
      compressr(text.tail, 0, text.head, 1, List.empty).reverse.mkString
    else
      ""
  }
}