package day21_1

import scala.io.Source

object Day21_1
{
  case class Grid(v: IndexedSeq[Boolean]) {
    val sz = math.sqrt(v.length).toInt

    def activePixels : Int = v.count(b => b)

    def row(i: Int) : IndexedSeq[Boolean] = v.slice(i * sz, (i + 1) * sz)

    def rotate : Grid = {
      val result = collection.mutable.IndexedSeq.fill(v.length)(false)
      for { y <- 0 until sz; x <- 0 until sz } result(x + y * sz) = v(y + (sz - (x + 1)) * sz)
      Grid(result)
    }

    def flip : Grid = {
      val result = collection.mutable.IndexedSeq.fill(v.length)(false)
      for { y <- 0 until sz; x <- 0 until sz } result(x + y * sz) = v((sz - (x + 1)) + y * sz)
      Grid(result)
    }

    private lazy val transforms = {
      val r1 = rotate
      val r2 = r1.rotate
      val r3 = r2.rotate
      val f = flip
      val fr1 = f.rotate
      val fr2 = fr1.rotate
      val fr3 = fr2.rotate
      Set(this, r1, r2, r3, f, fr1, fr2, fr3)
    }

    def matchTransform(other: Grid) : Boolean = other.transforms.contains(this)

    def subGrid(i: Int, gridSize: Int) : Grid = {
      val numGrids = (sz * sz) / (gridSize * gridSize)
      val gridX = i % math.sqrt(numGrids).toInt
      val gridY = i / math.sqrt(numGrids).toInt
      val result = collection.mutable.IndexedSeq.fill(gridSize * gridSize)(false)
      for {y <- 0 until gridSize; x <- 0 until gridSize } result(x + y * gridSize) = v((gridX * gridSize + x) + (gridY * gridSize + y) * sz)
      Grid(result)
    }

    def split : Seq[Grid] = {
      if (sz % 2 == 0) {
        // split into 2x2 subgrids
        val numGrids = sz * sz / 4
        (0 until numGrids).map(subGrid(_, 2))
      }
      else {
        // split into 3x3 subgrids
        val numGrids = sz * sz / 9
        (0 until numGrids).map(subGrid(_, 3))
      }
    }
  }

  def mergeGrids(grids: Seq[Grid]) : Grid = {
    val ns = math.sqrt(grids.length).toInt
    val gridRows = grids.head.sz
    var result = IndexedSeq[Boolean]()

    for { y <- 0 until ns } {
       for { i <- 0 until gridRows } {
         for { x <- 0 until ns } {
           result = result ++ grids(x + y * ns).row(i)
         }
       }
    }
    Grid(result)
  }

  def parseGrid(line: String) : Grid =
  {
    Grid(line.filter(_ != '/').map(_ == '#'))
  }

  def countActivatedPixels(lines: Seq[String], iterations: Int) : Int =
  {
    val grid = parseGrid(".#./..#/###")

    val enhancementMap = lines.map(l => {
      val tokens = l.split(" => ")
      (parseGrid(tokens(0)), parseGrid(tokens(1)))
    }).toMap

    evolveGrid(grid, enhancementMap, iterations)
  }

  def evolveGrid(grid: Grid, enhancementMap: Map[Grid, Grid], counter: Int) : Int = {
    if (counter == 0) grid.activePixels else {
      val subGrids = grid.split
      val enhancedSubGrids = subGrids.map(subGrid => {
        val key = enhancementMap.keySet.find(k => k.matchTransform(subGrid)).get
        enhancementMap(key)
      })
      evolveGrid(mergeGrids(enhancedSubGrids), enhancementMap, counter - 1)
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(parseGrid("#.#/..#/#..") == parseGrid("#.#/..#/#.."))
      assert(parseGrid(".#./..#/###").rotate == parseGrid("#../#.#/##."))
      assert(parseGrid(".#./..#/###").flip == parseGrid(".#./#../###"))

      assert(parseGrid("#.##...#./.#...#.#./###...#../..#....../#....###./##.##.###/....####./.###....#/......#..").subGrid(7,3) == parseGrid(".##/#../..."))
      assert(parseGrid("#.##...#./.#...#.#./###...#../..#....../#....###./##.##.###/....####./.###....#/......#..").split(7) == parseGrid(".##/#../..."))

      assert(mergeGrids(Seq(
        parseGrid("#.#/.#./###"), parseGrid("#../..#/..."), parseGrid(".#./.#./#.."),
        parseGrid("..#/#../##."), parseGrid(".../..#/##."), parseGrid(".../##./###"),
        parseGrid(".../.##/..."), parseGrid(".##/#../..."), parseGrid("##./..#/#..")
      )) == parseGrid("#.##...#./.#...#.#./###...#../..#....../#....###./##.##.###/....####./.###....#/......#.."))

      assert(countActivatedPixels(Seq(
        "../.# => ##./#../...",
        ".#./..#/### => #..#/..../..../#..#"
      ), 2) == 12)
      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day21.txt").getLines.toSeq

    println(countActivatedPixels(lines, 5))
  }
}
