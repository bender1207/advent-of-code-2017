package day18_1

import scala.io.Source

object Day18_1
{
  def findFirstRecoveredFrequency(lines: Seq[String]) : Long =
  {
    execute(lines, 0, Map[String, Long](), 0)
  }

  def command(instruction: String) : String = instruction.split(" ").head
  def arg1(instruction: String) : String = instruction.split(" ")(1)
  def arg2(instruction: String) : String = instruction.split(" ")(2)

  def isRegister(s: String) : Boolean = s.length == 1 && s(0).toInt > 57
  def value(registerMap: Map[String, Long], r: String) : Long = if (isRegister(r)) registerMap.getOrElse(r, 0) else r.toLong

  def execute(instructions: Seq[String], i: Long, regMap: Map[String, Long], freq: Long) : Long =
  {
    val instruction = instructions(i.toInt)
    command(instruction) match {
      case "snd" => {
        val r = arg1(instruction)
        execute(instructions, i + 1, regMap, value(regMap, r))
      }
      case "set" => {
        val r = arg1(instruction)
        val v = value(regMap, arg2(instruction))
        execute(instructions, i + 1, regMap + (r -> v), freq)
      }
      case "add" => {
        val r = arg1(instruction)
        val v1 = value(regMap, r)
        val v2 = value(regMap, arg2(instruction))
        execute(instructions, i + 1, regMap + (r -> (v1 + v2)), freq)
      }
      case "mul" => {
        val r = arg1(instruction)
        val v1 = value(regMap, r)
        val v2 = value(regMap, arg2(instruction))
        execute(instructions, i + 1, regMap + (r -> (v1 * v2)), freq)
      }
      case "mod" => {
        val r = arg1(instruction)
        val v1 = value(regMap, r)
        val v2 = value(regMap, arg2(instruction))
        execute(instructions, i + 1, regMap + (r -> (v1 % v2)), freq)
      }
      case "rcv" => {
        if (value(regMap, arg1(instruction)) != 0)
          freq
        else
          execute(instructions, i + 1, regMap, freq)
      }
      case "jgz" => {
        if (value(regMap, arg1(instruction)) != 0) {
          val v = value(regMap, arg2(instruction))
          execute(instructions, i + v, regMap, freq)
        }
        else
          execute(instructions, i + 1, regMap, freq)
      }
    }
  }


  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(findFirstRecoveredFrequency(Seq(
        "set a 1",
        "add a 2",
        "mul a a",
        "mod a 5",
        "snd a",
        "set a 0",
        "rcv a",
        "jgz a -1",
        "set a 1",
        "jgz a -2")) == 4)

      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day18.txt").getLines.toSeq

    println(findFirstRecoveredFrequency(lines))
  }
}
