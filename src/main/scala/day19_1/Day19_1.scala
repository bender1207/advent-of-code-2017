package day19_1

import scala.io.Source

object Day19_1
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

  def findLetters(lines: Seq[String]) : String = {
    val grid = new Grid(lines)
    val pos = grid.toPos(lines.head.indexOf('|'))
    travel(grid, pos, Down(), Seq())
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

  def travel(g: Grid, pos: Position, dir: Direction, letters: Seq[Char]) : String = {
    g(pos).get match {
      case '-' => travel(g, pos + dir.step(), dir, letters)
      case '|' => travel(g, pos + dir.step(), dir, letters)
      case '+' => {
        val newDir = turn(g, pos, dir)
        travel(g, pos + newDir.step(), newDir, letters)
      }
      case c if c >= 'A' && c <= 'Z' => {
        if (pathEnds(g, pos, dir))
          (letters ++ Seq(c)).mkString
        else
          travel(g, pos + dir.step(), dir, letters ++ Seq(c))
      }
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(findLetters(Seq(
        "     |          ",
        "     |  +--+    ",
        "     A  |  C    ",
        " F---|----E|--+ ",
        "     |  |  |  D ",
        "     +B-+  +--+ ",
        "                ")) == "ABCDEF")

      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day19.txt").getLines.toSeq

    println(findLetters(lines))
  }
}
