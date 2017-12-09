package day09_1

import scala.io.Source

object Day09_1
{

  def computeScore(str: String) : Int =
  {
    def scoreGroup(chars: List[Char], inputPosition: Int, depth: Int) : (Int, Int) =
    {
      assert(chars(inputPosition) == '{')

      var pos = inputPosition + 1
      var score = depth
      var isGarbage = false

      while (chars(pos) != '}' || isGarbage) {
        chars(pos) match {
          case '<' => isGarbage = true
          case '>' => isGarbage = false
          case '!' => pos = pos + 1
          case '{' =>
          {
            if (!isGarbage) {
              val pair: (Int, Int) = scoreGroup(chars, pos, depth + 1)
              score = score + pair._1
              pos = pair._2
            }
          }
          case _ =>
        }
        pos = pos + 1
      }

      (score, pos)
    }

    val chars : List[Char] = str.toList
    scoreGroup(chars, 0, 1)._1
  }


  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(computeScore("{}") == 1)
      assert(computeScore("{{{}}}") == 6)
      assert(computeScore("{{},{}}") == 5)
      assert(computeScore("{{{},{},{{}}}}") == 16)
      assert(computeScore("{<a>,<a>,<a>,<a>}") == 1)
      assert(computeScore("{{<ab>},{<ab>},{<ab>},{<ab>}}") == 9)
      assert(computeScore("{{<!!>},{<!!>},{<!!>},{<!!>}}") == 9)
      assert(computeScore("{{<a!>},{<a!>},{<a!>},{<ab>}}") == 3)
    }

    // Read input
    //
    val line: String = Source.fromResource("day09.txt").getLines.toSeq.mkString

    println(computeScore(line))
  }

}
