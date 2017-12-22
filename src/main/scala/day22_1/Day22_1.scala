package day22_1

import scala.io.Source

object Day22_1
{
  case class Pos(x: Int, y: Int) { def +(p: Pos) : Pos = Pos(this.x + p.x, this.y + p.y) }

  trait Dir {
    def step() : Pos
    def turnLeft() : Dir
    def turnRight() : Dir
  }
  case class Up() extends Dir {
    override def step(): Pos = Pos(0, -1)
    override def turnLeft(): Dir = Left()
    override def turnRight(): Dir = Right()
  }
  case class Down() extends Dir {
    override def step(): Pos = Pos(0, 1)
    override def turnLeft(): Dir = Right()
    override def turnRight(): Dir = Left()
  }
  case class Left() extends Dir {
    override def step(): Pos = Pos(-1, 0)
    override def turnLeft(): Dir = Down()
    override def turnRight(): Dir = Up()
  }
  case class Right() extends Dir {
    override def step(): Pos = Pos(1, 0)
    override def turnLeft(): Dir = Up()
    override def turnRight(): Dir = Down()
  }

  def countInfections(lines: Seq[String], bursts: Int) : Int =
  {
    var infected = Set[Pos]()
    var clean = Set[Pos]()

    lines.zipWithIndex.foreach( { case (line, y) => line.zipWithIndex.foreach( {
      case (c, x) => {
        if (c == '#')
          infected = infected + Pos(x, y)
        else
          clean = clean + Pos(x, y)
      }})})

    val startPos = Pos(lines.head.length / 2, lines.length / 2)
    val startDir = Up()

    simulate(startPos, startDir, infected, clean, 0, bursts, 0)
  }

  def simulate(pos: Pos, dir: Dir, infected: Set[Pos], clean: Set[Pos], infections: Int, bursts: Int, counter: Int) : Int =
  {
    if (counter == bursts) infections else {
      if (infected.contains(pos))
        simulate(pos + dir.turnRight().step(), dir.turnRight(), infected - pos, clean + pos, infections, bursts, counter + 1)
      else
        simulate(pos + dir.turnLeft().step(), dir.turnLeft(), infected + pos, clean - pos, infections + 1, bursts, counter + 1)
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(countInfections(Seq(
        "..#",
        "#..",
        "..."
      ), 10000) == 5587)
      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day22.txt").getLines.toSeq

    println(countInfections(lines, 10000))
  }
}
