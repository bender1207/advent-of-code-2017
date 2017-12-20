package day17_1

import scala.io.Source

object Day17_1
{
  def findSpinLockValue(steps: Int) : Int =
  {
    makeInsertion(IndexedSeq(0), 0, 0, steps)
  }

  def getUpdatedBuffer(buffer: IndexedSeq[Int], index: Int, value: Int) : IndexedSeq[Int] =
  {
    buffer.take(index + 1) ++ IndexedSeq(value) ++ buffer.drop(index + 1)
  }

  def makeInsertion(buffer: IndexedSeq[Int], counter: Int, prevIndex: Int, steps: Int) : Int =
  {
    if (counter == 2017) buffer((prevIndex + 2) % buffer.length) else {
      val index = (prevIndex + 1 + steps) % buffer.length
      val updatedBuffer = getUpdatedBuffer(buffer, index, counter + 1)
      makeInsertion(updatedBuffer, counter + 1, index, steps)
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(getUpdatedBuffer(IndexedSeq(0), 0, 1) == Seq(0, 1))
      assert(getUpdatedBuffer(IndexedSeq(1, 2, 3, 4), 1, 8) == Seq(1, 2, 8, 3, 4))
      assert(findSpinLockValue(3) == 638)
      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val line: String = Source.fromResource("day17.txt").getLines.toSeq.head.trim

    println(findSpinLockValue(line.toInt))
  }
}
