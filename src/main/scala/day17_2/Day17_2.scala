package day17_2

import scala.io.Source

object Day17_2
{
  class Node(v_ : Int)
  {
    val v: Int = v_
    var next: Option[Node] = None
  }

  def findSpinLockValue(steps: Int, stop: Int) : Int =
  {
    val node = new Node(0)
    node.next = Some(node)

    makeInsertion(node, 0, steps, stop)
  }

  def findValueAfterZero(node: Node) : Int = {
    if (node.v == 0) node.next.get.v else findValueAfterZero(node.next.get)
  }

  def getNodeAfterSteps(node: Node, steps: Int) : Node = {
    if (steps == 0) node else getNodeAfterSteps(node.next.get, steps - 1)
  }

  def makeInsertion(node: Node, counter: Int, steps: Int, stop: Int) : Int =
  {
    if (counter == stop) {
      findValueAfterZero(node)
    }
    else {
      val targetNode: Node = getNodeAfterSteps(node, steps)

      val nodeToInsert = new Node(counter + 1)
      nodeToInsert.next = targetNode.next
      targetNode.next = Some(nodeToInsert)

      makeInsertion(nodeToInsert, counter + 1, steps, stop)
    }
  }

  def main(args: Array[String]) =
  {
    // Read input
    //
    val line: String = Source.fromResource("day17.txt").getLines.toSeq.head.trim
    println(findSpinLockValue(line.toInt, 50000000))
  }
}
