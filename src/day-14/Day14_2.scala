package day14_2

import scala.io.Source

object Day14_2
{
  class DisjointSet
  {
    private var tree = Map[Int, Int]()

    def makeSet(v: Int) : Unit = if (! contains(v)) tree = tree + (v -> v)

    def contains(v: Int) : Boolean = tree.contains(v)

    def find(v: Int) : Int = {
      assert(tree.contains(v))
      if (tree(v) != v)
        tree = tree + (v -> find(tree(v)))
      tree(v)
    }

    def union(x: Int, y: Int) : Unit = {
      val xRoot = find(x)
      val yRoot = find(y)
      if (xRoot != yRoot) tree = tree + (yRoot -> xRoot)
    }

    def roots() : Set[Int] = tree.keySet.map(find)
  }

  def toBitStringFromHexString(hexString: String) : String = {
    hexString.map(c => "%4s".format(Integer.toString(Integer.parseInt(c.toString, 16), 2)).replace(' ', '0')).mkString
  }

  def makeBitString(line: String) : String = {
    val hexString = (0 until 128).map(i => day10_2.Day10_2.compute(line + "-" + i.toString)).mkString
    toBitStringFromHexString(hexString)
  }

  def makeSquareGrid(bitString: String) : IndexedSeq[Boolean] = {
    bitString.map(c => if (c.toString == "0") false else true)
  }

  def connectedComponentLabeling(grid : IndexedSeq[Boolean], xs: Int) : (IndexedSeq[Int], DisjointSet) =
  {
    def toIndex(x: Int, y: Int) = x + y * xs
    def toCoord(i: Int) = (i % xs, i / xs)
    def inGrid(x: Int, y: Int) : Boolean = x >= 0 && x < xs && y >= 0 && y < xs
    def getIndexIfInRegion(x: Int, y:Int) : Option[Int] = if (inGrid(x, y) && grid(toIndex(x, y))) Some(toIndex(x, y)) else None

    assert(grid.size == xs * xs)

    val linked = new DisjointSet
    val labels = collection.mutable.IndexedSeq.fill(grid.size){0}
    var nextLabel = 1

    // First pass
    for (i <- grid.indices) {
      if (grid(i)) {
        val (x, y) = toCoord(i)
        val neighbourIndices = Seq(getIndexIfInRegion(x, y - 1), getIndexIfInRegion(x - 1, y)).flatten

        if (neighbourIndices.isEmpty) {
          linked.makeSet(nextLabel)
          labels(i) = nextLabel
          nextLabel = nextLabel + 1
        }
        else {
          val neighbourLabels = neighbourIndices.map(labels(_))
          labels(i) = neighbourLabels.min
          neighbourLabels.foreach(linked.union(labels(i), _))
        }
      }
    }

    // Second pass (not really necessary when only need to count regions)
    for (i <- grid.indices) {
      if (grid(i)) {
        labels(i) = linked.find(labels(i))
      }
    }

    (labels, linked)
  }

  def countRegionsFromInput(line: String) : Int = {
    val bitString = makeBitString(line)
    countRegions(bitString, 128)
  }

  def countRegions(bitString: String, xs: Int) : Int = {
    val grid = makeSquareGrid(bitString)
    val (labels, linked) = connectedComponentLabeling(grid, xs)
    linked.roots().size
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(toBitStringFromHexString("af01") == "1010111100000001")
      assert(toBitStringFromHexString("fe2a") == "1111111000101010")

      assert(countRegions(Seq(
        "01001100",
        "01010011",
        "01111110",
        "00100001",
        "11101011",
        "00001000",
        "01011010",
        "00110110").mkString, 8) == 6)
    }
    //--------------------------

    // Read input
    //
    val line: String = Source.fromResource("day14.txt").getLines.toSeq.head.trim

    println(countRegionsFromInput(line))
  }
}
