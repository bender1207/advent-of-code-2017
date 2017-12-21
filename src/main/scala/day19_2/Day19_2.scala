package day19_2

import scala.io.Source

object Day19_2
{
  class Grid(lines: Seq[String])
  {
    val xs = lines.head.length
    val ys = lines.length

    def createBuffer(lines: List[String], buf: IndexedSeq[Char]) : IndexedSeq[Char] = {
      lines match {
        case Nil => buf
        case v :: vs => {
          assert(v.length == xs)
          createBuffer(vs, buf ++ v.toSeq)
        }
      }
    }

    def toPos(i: Int) : Position = Position(i % xs, i / xs)
    def toIndex(pos: Position) : Int = pos.x + pos.y * xs
    def isInside(pos: Position) : Boolean = pos.x >= 0 && pos.x < xs && pos.y >= 0 && pos.y < ys
    def apply(pos: Position) : Option[Char] = if (isInside(pos)) Some(buf(toIndex(pos))) else None

    private val buf = createBuffer(lines.toList, IndexedSeq()) : IndexedSeq[Char]
  }

  case class Position(x: Int, y: Int) {
    def +(p: Position) : Position = Position(this.x + p.x, this.y + p.y)
  }

  trait Direction {
    def step() : Position
    def opposite() : Direction
  }
  case class Up() extends Direction {
    override def step(): Position = Position(0, -1)
    override def opposite(): Direction = Down()
  }
  case class Down() extends Direction {
    override def step(): Position = Position(0, 1)
    override def opposite(): Direction = Up()
  }
  case class Left() extends Direction {
    override def step(): Position = Position(-1, 0)
    override def opposite(): Direction = Right()
  }
  case class Right() extends Direction {
    override def step(): Position = Position(1, 0)
    override def opposite(): Direction = Left()
  }

  def countSteps(lines: Seq[String]) : Int = {
    val grid = new Grid(lines)
    val pos = grid.toPos(lines.head.indexOf('|'))
    travel(grid, pos, Down(), 0)
  }

  def pathEnds(g: Grid, pos: Position, dir: Direction) : Boolean = {
    if (g(pos + dir.step()).isEmpty) true else {
      val c = g(pos + dir.step()).get
      c match {
        case '-' => false
        case '|' => false
        case '+' => false
        case _ => true
      }
    }
  }

  def turn(g: Grid, pos: Position, dir: Direction) : Direction = {
    Seq[Direction](Up(), Down(), Left(), Right()).filter(_ != dir.opposite()).filter((d) => g(pos + d.step()).get != ' ').head
  }

  def travel(g: Grid, pos: Position, dir: Direction, counter: Int) : Int = {
    g(pos).get match {
      case '-' => travel(g, pos + dir.step(), dir, counter + 1)
      case '|' => travel(g, pos + dir.step(), dir, counter + 1)
      case '+' => {
        val newDir = turn(g, pos, dir)
        travel(g, pos + newDir.step(), newDir, counter + 1)
      }
      case c if c >= 'A' && c <= 'Z' => {
        if (pathEnds(g, pos, dir))
          counter + 1
        else
          travel(g, pos + dir.step(), dir, counter + 1)
      }
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(countSteps(Seq(
        "     |          ",
        "     |  +--+    ",
        "     A  |  C    ",
        " F---|----E|--+ ",
        "     |  |  |  D ",
        "     +B-+  +--+ ",
        "                ")) == 38)

      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day19.txt").getLines.toSeq

    println(countSteps(lines))
  }

}
