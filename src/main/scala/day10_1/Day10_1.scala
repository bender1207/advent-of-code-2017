package day10_1

import scala.io.Source

object Day10_1
{
  def reverseSegment(values: IndexedSeq[Int], pos: Int, length: Int) : IndexedSeq[Int] =
  {
    val shifted = values.drop(pos) ++ values.take(pos)                                       // Shift values to get pos at index 0
    val shiftedReversed = shifted.take(length).reverse ++ shifted.slice(length, values.size) // Reverse first length values
    shiftedReversed.drop(values.size - pos) ++ shiftedReversed.take(values.size - pos)       // Shift back
  }

  def compute(line: String, numElements: Int) : Int =
  {
    var values : IndexedSeq[Int] = 0 until numElements
    val lengths = line.split(",").map(_.toInt)

    var pos = 0
    var skip_size = 0
    lengths.foreach(length => {
      values = reverseSegment(values, pos, length)
      pos = (pos + length + skip_size) % values.size
      skip_size = skip_size + 1
    })

    values(0) * values(1)
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(compute("3,4,1,5", 5) == 12)
    }

    // Read input
    //
    val line: String = Source.fromResource("day10.txt").getLines.toSeq.head

    println(compute(line, 256))
  }


}
