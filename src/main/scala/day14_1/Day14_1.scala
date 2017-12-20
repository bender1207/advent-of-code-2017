package day14_1

import scala.io.Source

object Day14_1
{
  def toHexString(line: String) : String = {
    (0 until 128).map(i => day10_2.Day10_2.compute(line + "-" + i.toString)).mkString
  }

  def toBitString(hexString: String) : String = {
    hexString.map(c => "%4s".format(Integer.toString(Integer.parseInt(c.toString, 16), 2)).replace(' ', '0')).mkString
  }

  def make128x128Grid(line: String) : IndexedSeq[Boolean] = {
    val hexString = toHexString(line)
    val bitString = toBitString(hexString)
    bitString.map(c => if (c.toString == "0") false else true)
  }

  def countUsedSquares(line: String) : Int = {
    make128x128Grid(line).count(b => b)
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(toBitString("0") == "0000")
      assert(toBitString("1") == "0001")
      assert(toBitString("2") == "0010")
      assert(toBitString("3") == "0011")
      assert(toBitString("4") == "0100")
      assert(toBitString("5") == "0101")
      assert(toBitString("6") == "0110")
      assert(toBitString("7") == "0111")
      assert(toBitString("8") == "1000")
      assert(toBitString("9") == "1001")
      assert(toBitString("a") == "1010")
      assert(toBitString("b") == "1011")
      assert(toBitString("c") == "1100")
      assert(toBitString("d") == "1101")
      assert(toBitString("e") == "1110")
      assert(toBitString("f") == "1111")
    }


    // Read input
    //
    val line: String = Source.fromResource("day14.txt").getLines.toSeq.head.trim

    println(countUsedSquares(line))
  }
}
