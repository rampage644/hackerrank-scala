import scala.io.Source.stdin
import math.max

object SwapNodes {
  sealed trait Tree
  case object Terminal extends Tree
  case class Node(left: Tree, right: Tree, value: Int = 1) extends Tree {
  }

  def swap(node: Tree): Tree = {
    node match {
      case Node(left, right, v) => Node(right, left, v)
      case Terminal => Terminal
    }
  }

  def depth(node: Tree) : Int = {
    node match {
      case Node(l, r, _) => max(depth(l), depth(r)) + 1
      case Terminal => 0
    }
  }


  def readTree(input: Seq[String]): Tree = {
    def createNode(children: String, nodes: Seq[String], currentDepth: Int): Tree = {
      val Array(l, r) = children.split(" ").map(_.toInt)
      val leftTree = l match {
        case -1 => Terminal
        case i => createNode(nodes(i-1), nodes, i)
      }

      val rightTree = r match {
        case -1 => Terminal
        case i => createNode(nodes(i-1), nodes, i)
      }

      Node(leftTree, rightTree, currentDepth)
    }

    createNode(input.head, input, 1)
  }

  def swappedAt(node: Tree, depth:Int, currentDepth:Int = 1): Tree = {
    node match {
      case Node(l, r, v) if currentDepth % depth == 0 => Node(swappedAt(r, depth, currentDepth+1), swappedAt(l, depth, currentDepth+1), v)
      case Node(l, r, v) => Node(swappedAt(l, depth, currentDepth+1), swappedAt(r, depth, currentDepth+1), v)
      case Terminal => Terminal
    }
  }

  def printTree(root: Tree): String = {
    def printIntoBuilder(root: Tree, builder: StringBuilder): Unit = {
      root match {
        case Terminal => ()
        case Node(l, r, v) => {
          printIntoBuilder(l, builder)
          builder.append(s"$v ")
          printIntoBuilder(r, builder)
        }
      }
    }

    val builder = new StringBuilder
    printIntoBuilder(root, builder)
    builder.mkString.dropRight(1)
  }

  def solve(it: Iterator[String]): Iterator[String] = {
    val nodesCount = it.next().toInt
    val nodes = Iterator.continually(it.next).take(nodesCount).toList
    var tree = readTree(nodes)
    val swapCount = it.next().toInt
    it.take(swapCount).map(_.toInt).map(count => {
      tree = swappedAt(tree, count)
      printTree(tree)
    })
  }

  def main(args: Array[String]) {
    solve(stdin.getLines).foreach(println)
  }
}
