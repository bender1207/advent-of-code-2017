package day18_2

import scala.collection.mutable
import scala.io.Source

object Day18_2
{
  case class Program(id: Int,
                     i: Long,
                     regMap: Map[String, Long],
                     valuesSent: Int,
                     sq: mutable.Queue[Long],
                     rq: mutable.Queue[Long])

  def countValuesSent(lines: Seq[String]) : Int =
  {
    val q0 = mutable.Queue[Long]()
    val q1 = mutable.Queue[Long]()

    val p0 = Program(0, 0, Map(("p", 0)), 0, q1, q0)
    val p1 = Program(1, 0, Map(("p", 1)), 0, q0, q1)

    val (p, other) = execute(lines, p0, p1)

    List(p, other).filter(_.id == 1).head.valuesSent
  }

  def command(instruction: String) : String = instruction.split(" ").head
  def arg1(instruction: String) : String = instruction.split(" ")(1)
  def arg2(instruction: String) : String = instruction.split(" ")(2)

  def isRegister(s: String) : Boolean = s.length == 1 && s(0).toInt > 57
  def value(registerMap: Map[String, Long], r: String) : Long = if (isRegister(r)) registerMap.getOrElse(r, 0) else r.toLong

  def isDeadlock(instructions: Seq[String], p1: Program, p2: Program) : Boolean = {
    command(instructions(p1.i.toInt)) == "rcv" && command(instructions(p2.i.toInt)) == "rcv" && p1.sq.isEmpty && p1.rq.isEmpty
  }

  def execute(instructions: Seq[String], p: Program, other: Program) : (Program, Program) =
  {
    val regMap = p.regMap

    val instruction = instructions(p.i.toInt)
    command(instruction) match {
      case "snd" => {
        val v = value(regMap, arg1(instruction))
        p.sq.enqueue(v)
        execute(instructions, other, Program(p.id, p.i + 1, p.regMap, p.valuesSent + 1, p.sq, p.rq))
      }
      case "set" => {
        val r = arg1(instruction)
        val v = value(regMap, arg2(instruction))
        execute(instructions, other, Program(p.id, p.i + 1, regMap + (r -> v), p.valuesSent, p.sq, p.rq))
      }
      case "add" => {
        val r = arg1(instruction)
        val v1 = value(regMap, r)
        val v2 = value(regMap, arg2(instruction))
        execute(instructions, other, Program(p.id, p.i + 1, regMap + (r -> (v1 + v2)), p.valuesSent, p.sq, p.rq))
      }
      case "mul" => {
        val r = arg1(instruction)
        val v1 = value(regMap, r)
        val v2 = value(regMap, arg2(instruction))
        execute(instructions, other, Program(p.id, p.i + 1, regMap + (r -> (v1 * v2)), p.valuesSent, p.sq, p.rq))
      }
      case "mod" => {
        val r = arg1(instruction)
        val v1 = value(regMap, r)
        val v2 = value(regMap, arg2(instruction))
        execute(instructions, other, Program(p.id, p.i + 1, regMap + (r -> (v1 % v2)), p.valuesSent, p.sq, p.rq))
      }
      case "rcv" => {
        if (isDeadlock(instructions, p, other)) (p, other) else {
          if (p.rq.isEmpty) {
            // Receive queue empty. Wait for other program to send message.
            execute(instructions, other, Program(p.id, p.i, p.regMap, p.valuesSent, p.sq, p.rq))
          }
          else {
            // Receive
            val r = arg1(instruction)
            val v = p.rq.dequeue()
            execute(instructions, other, Program(p.id, p.i + 1, regMap + (r -> v), p.valuesSent, p.sq, p.rq))
          }
        }
      }
      case "jgz" => {
        if (value(regMap, arg1(instruction)) > 0) {
          val v = value(regMap, arg2(instruction))
          execute(instructions, other, Program(p.id, p.i + v, regMap, p.valuesSent, p.sq, p.rq))
        }
        else
          execute(instructions, other, Program(p.id, p.i + 1, regMap, p.valuesSent, p.sq, p.rq))
      }
    }
  }


  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(countValuesSent(Seq(
        "snd 1",
        "snd 2",
        "snd p",
        "rcv a",
        "rcv b",
        "rcv c",
        "rcv d")) == 3)

      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day18.txt").getLines.toSeq

    println(countValuesSent(lines))
  }
}
