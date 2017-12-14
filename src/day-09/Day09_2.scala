package day09_2

import scala.io.Source

object Day09_2
{

  def countGarbageCharacters(str: String) : Int =
  {
    def countGarbageCharactersInGroup(chars: List[Char], inputPosition: Int) : (Int, Int) =
    {
      assert(chars(inputPosition) == '{')

      var pos = inputPosition + 1
      var score = 0
      var isGarbage = false

      while (chars(pos) != '}' || isGarbage) {
        chars(pos) match {
          case '<' => {
            if (isGarbage) score = score + 1
            isGarbage = true
          }
          case '>' => isGarbage = false
          case '!' => pos = pos + 1
          case '{' =>
          {
            if (!isGarbage) {
              val pair: (Int, Int) = countGarbageCharactersInGroup(chars, pos)
              score = score + pair._1
              pos = pair._2
            }
            else score = score + 1
          }
          case _ => if (isGarbage) score = score + 1
        }
        pos = pos + 1
      }

      (score, pos)
    }

    val chars : List[Char] = str.toList
    countGarbageCharactersInGroup(chars, 0)._1
  }


  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(countGarbageCharacters("{<>}") == 0)
      assert(countGarbageCharacters("{<random characters>}") == 17)
      assert(countGarbageCharacters("{<<<<>}") == 3)
      assert(countGarbageCharacters("{<{!>}>}") == 2)
      assert(countGarbageCharacters("{<!!>}") == 0)
      assert(countGarbageCharacters("{<!!!>>}") == 0)
      assert(countGarbageCharacters("{<{o\"i!a,<{i<a>}") == 10)
    }

    // Read input
    //
    val line: String = Source.fromResource("day09.txt").getLines.toSeq.mkString

    println(countGarbageCharacters(line))
  }

}
