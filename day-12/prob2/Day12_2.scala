package day12_2

import scala.io.Source

object Day12_2
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

  def countGroups(rows: Seq[String]) : Int =
  {
    val programMap = toMap(rows)

    val emptySeq : Seq[Set[Int]] = Seq()
    val allGroups = programMap.keySet.foldLeft(emptySeq)((groups: Seq[Set[Int]], program: Int) => {
      if (groups.forall(!_.contains(program))) {
        groups ++ Seq(travel(programMap, program, Set()))
      }
      else {
        groups
      }
    })

    allGroups.size
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

      assert(countGroups(rows) == 2)
    }

    // Read input
    //
    val rows: Seq[String] = Source.fromResource("day12.txt").getLines.toSeq

    println(countGroups(rows))
  }

}
