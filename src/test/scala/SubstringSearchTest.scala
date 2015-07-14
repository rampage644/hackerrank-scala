import SubstringSearch._
import org.scalatest.{FunSuite, Matchers}

class SubstringSearchTest extends  FunSuite with Matchers {
  test("kmp search") {
    kmpSearch("abcdef", "def") should be (true)
    kmpSearch("computer", "muter") should be (false)
    kmpSearch("stringmatchingmat", "ingmat") should be (true)
    kmpSearch("videobox", "videobox") should be (true)

    kmpSearch("a" * 100000, "a" * (100000-1) + "b")

    kmpSearch("asdhgjsah", "d") should be (true)
    kmpSearch("asdhgjsah", "js") should be (true)
    kmpSearch("asdhgjsah", "") should be (false)
    kmpSearch("", "sdasdas") should be (false)
  }

  test("building table") {
    buildTable("abcdabd") should be (Array(-1,0,0,0,0,1,2))
    buildTable("participate in parachute") should be (Array(-1,0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,1,2,3,0,0,0,0,0))
  }

  test("testable main") {
    val in = """4
               |abcdef
               |def
               |computer
               |muter
               |stringmatchingmat
               |ingmat
               |videobox
               |videobox""".stripMargin.split("\n")
    val out = """YES
                |NO
                |YES
                |YES""".stripMargin.split("\n")

    testable_main(in.toIterator).toArray should be (out)
  }

  test("input051.txt") {
//    val in = "pbnegxcctakvuzlkfgalplyyxymxteenegpurdzqahowsfhqcibixfzjcozegqguoqntgcouxeaabbdkzcfjmaomblvbutwjuhuvhagqfhtxpgghtlomdjcyhwmmbgswusqplvziqqzkptxtawswqqvjnqjpghhayctjnqgdvwculprcuioawxwbgskvkbvkxsxxkcbbkyskaepzaannhanmrpkpzstzoidmgoxyogiwyybbgshfeacaksyvrmilhkcylenuvokshjdnahddlqaqgzhfvgazxdgxgvferoaqyutuvbvadopgmnjuvesbuljlokiyexszumpgehuiswurhricrtcjbmkvcsbhuyaykkbnkjrqisstokaensjqtheznoxnlzqqyrboalzhxqwzcjcapntqnheyykgaitsghmviupfhqwokieomp"
//    val out = "enegpurdzqahowsfhqcibixfzjcozegqguoqntgcouxeaabbdkzcfjmaomblvbutwjuhuvhagqfhtxpgghtlomdjcyhwmmbgswusqplvziqqzkptxtawswqqvjnqjpghhayctjnqgdvwculprcuioawxwbgskvkbvkxsxxkcbbkyskaepzaannhanmrpkpzstzoidmgoxyogiwyybbgshfeacaksyvrmilhkcylenuvokshjdnahddlqaqgzhfvga"

//    val in = "fgghij"
//    val out = "ghi"
//    kmpSearch(in, out) should be (true)


    val in = scala.io.Source.fromURL(getClass getResource "input051.txt") getLines
    val out = scala.io.Source.fromURL(getClass getResource "output051.txt") getLines

    testable_main(in).toArray should be (out.toArray)
  }

}
