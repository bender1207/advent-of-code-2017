package day15_1

import scala.io.Source

object Day15_1
{
  def countCommonLowest16Bits(lines: Seq[String]) : Int =
  {
    val startA = lines.head.split(" ").last.toInt
    val startB = lines.last.split(" ").last.toInt

    count(startA, startB, 0)
  }

  def compare(a: Int, b: Int) : Boolean =
  {
    val aBits = "%32s".format(Integer.toString(a, 2)).replace(' ', '0')
    val bBits = "%32s".format(Integer.toString(b, 2)).replace(' ', '0')

    aBits.substring(16) == bBits.substring(16)
  }

  def count(a: Int, b: Int, iterationCounter: Int) : Int =
  {
    if (iterationCounter == 40000000) {
      0
    }
    else {
      val aNew = ((a.toLong * 16807) % 2147483647).toInt
      val bNew = ((b.toLong * 48271) % 2147483647).toInt

      val equal = compare(aNew, bNew)
      if (equal) {
        1 + count(aNew, bNew, iterationCounter + 1)
      }
      else {
        count(aNew, bNew, iterationCounter + 1)
      }
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(countCommonLowest16Bits(Seq("Generator A starts with 65", "Generator B starts with 8921")) == 588)
      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day15.txt").getLines.toSeq

    println(countCommonLowest16Bits(lines))
  }
}
