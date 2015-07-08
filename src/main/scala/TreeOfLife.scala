import scala.collection.mutable.HashMap
import scala.io.Source.stdin
import scala.annotation.tailrec


object TreeOfLife {

  sealed trait Tree
  case class Node(left:Tree, right:Tree, state:Boolean) extends Tree
  case object Terminal extends Tree

  val ON = Node(Terminal, Terminal, true)
  val OFF = Node(Terminal, Terminal, false)

  type Rule = (Boolean, Boolean, Boolean, Boolean) => Boolean
  type Path = Node => Boolean

  def parseTree(input:String):Tree = {
    @tailrec
    def parseTreeHelper(input:String, state:Int, count:Int, pos:Int):Int = {
      val sym = input.head
      val new_state = sym match {
        case ' ' if state == 0 => 1
        case ' ' if state == 2 => 3
        case '.' if state == 1 => 2
        case 'X' if state == 1 => 2
        case _ => 0
      }
      val new_count = count + (sym match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      })

      if (new_state == 3 && new_count == 1)
        pos
      else
        parseTreeHelper(input.tail, new_state, new_count, pos +1)
    }

    if (input == "X")
      return ON
    if (input == ".")
      return OFF

    val centerPos = parseTreeHelper(input, 0, 0, 0)
    val (l, state, r) = (input.substring(1, centerPos-2), input(centerPos-1), input.substring(centerPos+1, input.length-1))
    Node(l match {
      case "X" => ON
      case "." => OFF
      case branch => parseTree(branch)
    },
    r match {
      case "X" => ON
      case "." => OFF
      case branch => parseTree(branch)
    },
    state == 'X')

  }

  def parseRule(input: Int): Rule = {
    (v0,v1,v2,v3) => {
      val (w0, w1, w2, w3) = (if (v0) 1 else 0, if (v1) 2 else 0, if (v2) 4 else 0, if (v3) 8 else 0)
      (input & math.pow(2, w0+w1+w2+w3).toInt) != 0
    }
  }

  def updateTree(src: Tree, rule:Rule): Tree = {

    def nodeToState(node:Tree) = node match {
      case Node(_,_,s) => s
      case Terminal => false
    }

    def updateTreeHelper(src:Tree, parent:Tree, rule:Rule): Tree = {
      src match {
        case self@Node(l, r, s) => Node(
            updateTreeHelper(l, self, rule),
            updateTreeHelper(r, self, rule),
            rule(
              nodeToState(r),
              s,
              nodeToState(l),
              nodeToState(parent)
            )
        )
        case Terminal => Terminal
      }
    }

    updateTreeHelper(src, Terminal, rule)
  }



  @tailrec
  def drillInto(input: String, node:Tree): Boolean = {
    val Node(l, r, state) = node
    if (input.isEmpty)
      state
    else
      drillInto(input.tail, if (input.head == '>') r else l)
  }

  @tailrec
  def evolve(src:Tree, iterations:Int, rule:Rule, population:HashMap[Int,Tree], lastKey:Int): Tree = {
    iterations match {
      case 0 => src
      case n => {
        val tree = updateTree(src, rule)
        population += lastKey+1 -> tree
        evolve(tree, n - 1, rule, population, lastKey+1)
      }
    }
  }

  def doTheJob(iterator: Iterator[String]): Iterator[String] = {
    val rule = parseRule(iterator.next().toInt)
    var oldestTree = parseTree(iterator.next())
    var oldestIteration = 0
    iterator.next() // discard next line
    var iterations = 0
    val population = HashMap[Int,Tree](oldestIteration -> oldestTree)

    def doit(input: String): String = {
      val Array(iterationsToDo, path) = input split " "
      iterations += iterationsToDo.toInt

      val evolvedTree = population.getOrElse(iterations, {
        oldestTree = evolve(oldestTree, iterations-oldestIteration, rule, population, oldestIteration)
        oldestIteration = scala.math.max(iterations, oldestIteration)
        oldestTree
      })
      if (drillInto(path.drop(1).dropRight(1), evolvedTree)) "X" else "."
    }

    iterator map doit
  }


  def main(args: Array[String]) {
    val it = stdin.getLines()

    doTheJob (it) foreach println
  }
}

