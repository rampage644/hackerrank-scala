import org.scalatest.{Matchers, FunSuite}
import SansaXor._
/**
  * Created by ramp on 2/4/16.
  */
class SansaXorTest extends FunSuite with Matchers {
  test("input01.txt") {
    val in = scala.io.Source.fromURL(getClass.getResource("sansaxor_input01.txt")).getLines()
    val out = scala.io.Source.fromURL(getClass.getResource("sansaxor_output01.txt")).getLines()

    doTheJob(in).toList should be (out.toList)
  }

  test("input07.txt") {
    val in = scala.io.Source.fromURL(getClass.getResource("sansaxor_input07.txt")).getLines()
    val out = scala.io.Source.fromURL(getClass.getResource("sansaxor_output07.txt")).getLines()

    doTheJob(in).toList should be (out.toList)
  }

  test("samples") {
    val input = """2
                  |3
                  |1 2 3
                  |4
                  |4 5 7 5""".stripMargin.split("\n").toIterator

    doTheJob(input).toList should be("2" :: "0" :: Nil)
  }
}
