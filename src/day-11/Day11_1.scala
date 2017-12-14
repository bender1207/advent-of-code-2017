package day11_1

import scala.io.Source

case class Hex(q: Int, r: Int)
{
  def +(other: Hex) = Hex(this.q + other.q, this.r + other.r)
  def distance : Int = (math.abs(q) + math.abs(q + r) + math.abs(r)) / 2
}

object Day11_1
{
  def getHexStep(step: String) : Hex =
  {
    step match {
      case "n" => Hex(0, -1)
      case "ne" => Hex(1, -1)
      case "se" => Hex(1, 0)
      case "s" => Hex(0, 1)
      case "sw" => Hex(-1, 1)
      case "nw" => Hex(-1, 0)
    }
  }

  def countDistance(line: String) : Int =
  {
    val steps = line.split(",").map(_.trim)
    steps.map(getHexStep).reduce(_ + _).distance
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(countDistance("ne,ne,ne") == 3)
      assert(countDistance("ne,ne,sw,sw") == 0)
      assert(countDistance("ne,ne,s,s") == 2)
      assert(countDistance("se,sw,se,sw,sw") == 3)
    }

    // Read input
    //
    val line: String = Source.fromResource("day11.txt").getLines.toSeq.head.trim

    println(countDistance(line))
  }
}
