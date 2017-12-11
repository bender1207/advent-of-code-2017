package day11_2

import scala.io.Source

case class Hex(q: Int, r: Int)
{
  def +(other: Hex) = Hex(this.q + other.q, this.r + other.r)
  def distance : Int = (math.abs(q) + math.abs(q + r) + math.abs(r)) / 2
}

object Day11_2
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
    val hexSteps = steps.map(getHexStep)

    hexSteps.foldLeft((0, Hex(0, 0)))((pair, step) => {
      val accHex = pair._2 + step
      (math.max(pair._1, accHex.distance), accHex)
    })._1
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(countDistance("ne,ne,ne") == 3)
      assert(countDistance("ne,ne,sw,sw") == 2)
      assert(countDistance("ne,ne,s,s") == 2)
    }

    // Read input
    //
    val line: String = Source.fromResource("day11.txt").getLines.toSeq.head.trim

    println(countDistance(line))
  }
}
