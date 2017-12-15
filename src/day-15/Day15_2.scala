package day15_2

import scala.io.Source

object Day15_2
{
  class Judge
  {
    private val aValues = collection.mutable.Queue[Int]()
    private val bValues = collection.mutable.Queue[Int]()

    var counter = 0
    var hits = 0

    def addA(v: Int) : Unit = aValues.enqueue(v)
    def addB(v: Int) : Unit = bValues.enqueue(v)

    def compare()
    {
      if (! (aValues.isEmpty || bValues.isEmpty)) {

        val a = aValues.dequeue()
        val b = bValues.dequeue()

        val aBits = "%32s".format(Integer.toString(a, 2)).replace(' ', '0')
        val bBits = "%32s".format(Integer.toString(b, 2)).replace(' ', '0')

        counter = counter + 1

        if (aBits.substring(16) == bBits.substring(16)) hits = hits + 1
      }
    }
  }

  def countCommonLowest16Bits(lines: Seq[String]) : Int =
  {
    var a = lines.head.split(" ").last.toInt
    var b = lines.last.split(" ").last.toInt

    val judge = new Judge

    while (judge.counter < 5000000) {
      a = ((a.toLong * 16807) % 2147483647).toInt
      b = ((b.toLong * 48271) % 2147483647).toInt

      if (a % 4 == 0) judge.addA(a)
      if (b % 8 == 0) judge.addB(b)

      judge.compare()
    }

    judge.hits
  }


  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(countCommonLowest16Bits(Seq("Generator A starts with 65", "Generator B starts with 8921")) == 309)
      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day15.txt").getLines.toSeq

    println(countCommonLowest16Bits(lines))
  }
}
