package day16_2

import scala.io.Source

object Day16_2
{
  def computeProgramOrder(line: String, letters: String, times: Long) : String =
  {
    val moves = line.split(",").map(_.trim).toList
    repeatDance(moves, letters, 0, times, Set(), IndexedSeq())
  }

  def repeatDance(moves: List[String], letters: String, counter: Long, times: Long, visited: Set[String], visitedIndexed: IndexedSeq[String]) : String =
  {
    if (counter == times) letters else {
      if (visited.contains(letters)) {
        val i = (times - (times / counter) * counter).toInt
        visitedIndexed(i)
      }
      else {
        val updatedVisited = visited + letters
        val updatedVisitedIndexed = visitedIndexed :+ letters

        repeatDance(moves, dance(moves, letters), counter + 1, times, updatedVisited, updatedVisitedIndexed)
      }
    }
  }

  def toInt(s: String) : Int = Integer.parseInt(s)

  def dance(moves: List[String], letters: String) : String =
  {
    moves match {
      case Nil => letters
      case m :: ms => {
        m.head match {
          case 's' => dance(ms, spin(letters, toInt(m.substring(1))))
          case 'x' => {
            val i = m.substring(1).split("/").map(toInt)
            dance(ms, exchange(letters, i(0), i(1)))
          }
          case 'p' => {
            val s = m.substring(1).split("/")
            dance(ms, partner(letters, s(0)(0), s(1)(0)))
          }
        }
      }
    }
  }

  def spin(letters: String, steps: Int) : String =
  {
    letters.drop(letters.length - steps) + letters.take(letters.length - steps)
  }

  def exchange(letters: String, i1: Int, i2: Int) : String =
  {
    val iMin = math.min(i1, i2)
    val iMax = math.max(i1, i2)
    val c1 = letters(iMin)
    val c2 = letters(iMax)

    letters.slice(0, iMin) + c2 + letters.slice(iMin + 1, iMax) + c1 + letters.slice(iMax + 1, letters.length)
  }

  def partner(letters: String, c1: Char, c2: Char) : String =
  {
    exchange(letters, letters.indexOf(c1), letters.indexOf(c2))
  }


  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(spin("abcde", 1) == "eabcd")
      assert(exchange("eabcd", 3, 4) == "eabdc")
      assert(partner("eabdc", 'e', 'b') == "baedc")

      assert(computeProgramOrder("s1,x3/4,pe/b", "abcde", 1) == "baedc")
      assert(computeProgramOrder("s1", "abcde", 11) == "eabcd")
      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val line: String = Source.fromResource("day16.txt").getLines.toSeq.head

    println(computeProgramOrder(line, "abcdefghijklmnop", 1000000000))
  }
}
