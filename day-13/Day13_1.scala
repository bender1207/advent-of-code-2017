package day13_1

import scala.io.Source

object Day13_1
{
  def scannerIndex(/*time*/ t: Int, /*range*/ r: Int) : Int =
  {
    val w = r - 1
    val i = t - (t / w) * w
    if ((t/w) % 2 == 0) i else w - i
  }

  def severity(rows: Seq[String]) : Int =
  {
    val depthMap = rows.map(r => {
      val pair = r.split(":")
      (pair(0).trim.toInt, pair(1).trim.toInt)
    }).toMap

    val time = 0 to depthMap.keySet.max
    time.map(t => { if (depthMap.contains(t) && scannerIndex(t, depthMap(t)) == 0) t * depthMap(t) else 0 }).sum
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

      assert(severity(rows) == 24)
    }

    // Read input
    //
    val rows: Seq[String] = Source.fromResource("day13.txt").getLines.toSeq

    println(severity(rows))
  }
}
