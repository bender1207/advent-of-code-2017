package day12_1

import scala.io.Source

object Day12_1
{
  def travel(programMap: Map[Int, Seq[Int]], current: Int, visited: Set[Int]) : Set[Int] =
  {
    if (visited.contains(current)) {
      visited
    }
    else {
      val updatedVisited = visited + current
      programMap(current).foldLeft(updatedVisited)((v: Set[Int], i: Int) => v.union(travel(programMap, i, v)))
    }
  }

  def toMap(rows: Seq[String]) : Map[Int, Seq[Int]] =
  {
    rows.flatMap(r => {
      val tokens = r.split(" ").toIndexedSeq
      tokens.slice(2, tokens.size).map(t => (tokens.head.toInt, t.trim.filterNot(",".toSet).toInt))
    }).groupBy(_._1).map({ case (k, v) => (k, v.map(_._2)) })
  }

  def countPrograms(rows: Seq[String]) : Int =
  {
    val programMap = toMap(rows)
    val visited = travel(programMap, 0, Set())
    visited.size
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      val rows = Seq(
        "0 <-> 2",
        "1 <-> 1",
        "2 <-> 0, 3, 4",
        "3 <-> 2, 4",
        "4 <-> 2, 3, 6",
        "5 <-> 6",
        "6 <-> 4, 5")

      assert(countPrograms(rows) == 6)
    }

    // Read input
    //
    val rows: Seq[String] = Source.fromResource("day12.txt").getLines.toSeq

    println(countPrograms(rows))
  }
}
