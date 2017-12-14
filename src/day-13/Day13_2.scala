package day12_2

import scala.io.Source

object Day13_2
{
  def scannerIndex(/*time*/ t: Int, /*range*/ r: Int) : Int =
  {
    val w = r - 1
    val i = t - (t / w) * w
    if ((t/w) % 2 == 0) i else w - i
  }

  def minDelay(rows: Seq[String]) : Int =
  {
    val depthMap = rows.map(r => {
      val pair = r.split(":")
      (pair(0).trim.toInt, pair(1).trim.toInt)
    }).toMap

    compDelay(0, depthMap)
  }

  def compDelay(dt: Int, depthMap: Map[Int, Int]) : Int =
  {
    val packetPosition = 0 to depthMap.keySet.max
    if (packetPosition.forall(pos => !depthMap.contains(pos) || scannerIndex(pos + dt, depthMap(pos)) != 0))
      dt
    else
      compDelay(dt + 1, depthMap)
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      val rows = Seq(
        "0: 3",
        "1: 2",
        "4: 4",
        "6: 4")

      assert(minDelay(rows) == 10)
    }

    // Read input
    //
    val rows: Seq[String] = Source.fromResource("day13.txt").getLines.toSeq

    println(minDelay(rows))
  }

}
