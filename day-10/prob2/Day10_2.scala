package day10_2

import scala.io.Source

object Day10_2
{

  def reverseSegment(values: IndexedSeq[Int], pos: Int, length: Int) : IndexedSeq[Int] =
  {
    val shifted = values.drop(pos) ++ values.take(pos)                                       // Shift values to get pos at index 0
    val shiftedReversed = shifted.take(length).reverse ++ shifted.slice(length, values.size) // Reverse first length values
    shiftedReversed.drop(values.size - pos) ++ shiftedReversed.take(values.size - pos)       // Shift back
  }

  def xor(values: List[Int]) : Int =
  {
    values match {
      case v :: tail => v ^ xor(tail)
      case Nil => 0
    }
  }

  def compute(line: String) : String =
  {
    var values : IndexedSeq[Int] = 0 until 256
    val lengths : IndexedSeq[Int] = line.map(_.toInt)
    val extendedLengths = lengths ++ Seq(17, 31, 73, 47, 23)

    var pos = 0
    var skip_size = 0

    (0 until 64).foreach(i =>
    {
      extendedLengths.foreach(length => {
        values = reverseSegment(values, pos, length)
        pos = (pos + length + skip_size) % values.size
        skip_size = skip_size + 1
      })
    })

    val xoredValues = for (i <- 0 until 16) yield xor(values.slice(i * 16, i * 16 + 16).toList)
    xoredValues.map(c => "%2s".format(Integer.toString(c, 16)).replace(' ', '0')).mkString
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(compute("") == "a2582a3a0e66e6e86e3812dcb672a272")
      assert(compute("AoC 2017") == "33efeb34ea91902bb2f59c9920caa6cd")
      assert(compute("1,2,3") == "3efbe78a8d82f29979031a4aa0b16a9d")
      assert(compute("1,2,4") == "63960835bcdc130f0b66d7ff4f6a5a8e")
    }

    // Read input
    //
    val line: String = Source.fromResource("day10.txt").getLines.toSeq.head.trim

    println(compute(line))
  }


}
