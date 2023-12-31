//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-core:2.10.0
import cats.syntax.all.*

enum Direction:
  case North, East, South, West
  def vertical: Boolean = this match
    case North | South => true
    case East | West => false

case class Grid(underlying: Vector[Vector[Char]], orientation: Direction):

  lazy val height = if orientation.vertical then underlying.size else underlying.head.size
  lazy val width = if orientation.vertical then underlying.head.size else underlying.size

  def cell(col: Int, row: Int): Char =
    orientation match
      case Direction.North => underlying(col)(row)
      case Direction.East => underlying(row)(underlying.head.size - 1 - col)
      case Direction.South => underlying(col)(underlying.size - 1 - row)
      case Direction.West => underlying(underlying.head.size - 1 - row)(col)

  def tilt: Grid =
    val tilted = for
      i <- (0 until height).toVector
    yield
      var j = 0
      var tilted = Vector.empty[Char]
      while j < height do
        val nextRock =
          (j until height).find(j => cell(i, j) == '#').getOrElse(-1)
        if nextRock < 0 then
          val numRound = (j until height).count(j => cell(i, j) == 'O')
          val numSpace = height - j - numRound
          tilted = tilted ++ Vector.fill(numRound)('O') ++ Vector.fill(numSpace)('.')
          j = height
        else
          val numRound = (j until nextRock).count(j => cell(i, j) == 'O')
          val numSpace = nextRock - j - numRound
          tilted = tilted ++ Vector.fill(numRound)('O') ++ Vector.fill(numSpace)('.') ++ Vector('#')
          j = nextRock + 1
      tilted
    Grid(tilted, Direction.North)

  def spin: Grid =
    tilt.rotateClockwise.tilt.rotateClockwise.tilt.rotateClockwise.tilt.rotateClockwise

  def rotateClockwise: Grid =
    val rotated = orientation match
      case Direction.North => Direction.East
      case Direction.East => Direction.South
      case Direction.South => Direction.West
      case Direction.West => Direction.North
    Grid(underlying, rotated)

  def totalLoad: Int =
    (0 until height).toList.foldMap: row =>
      (0 until width).map(col => cell(col, row)).count(_ == 'O') * (height - row)

  def dump: Unit =
    println((0 until height).map: r =>
      (0 until width).map: c =>
        cell(c, r)
      .mkString("")
    .mkString("\n"))

def part1(input: String): Int =
  val grid = Grid(input.linesIterator.map(_.toVector).toVector.transpose, Direction.North)
  grid.tilt.totalLoad

def part2(input: String): Int =
  val grid = Grid(input.linesIterator.map(_.toVector).toVector.transpose, Direction.North)
  def loop(grid: Grid, remaining: Long, chain: Vector[Grid]): Grid =
    if remaining <= 0 then grid
    else
      if chain.contains(grid) then
        val cycleStart = chain.indexOf(grid)
        val cycleLength = chain.size - cycleStart
        chain((remaining % cycleLength).toInt + cycleStart)
      else
        val spun = grid.spin
        loop(spun, remaining - 1, chain :+ grid)
  loop(grid, 1000000000L, Vector.empty).totalLoad

val sample = """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""

assert(part1(sample) == 136)
println("Sample passes part 1")

val parsedSample = Grid(sample.linesIterator.map(_.toVector).toVector.transpose, Direction.North)
println("--- Sample")
parsedSample.dump
println("--- Sample Rotated Once")
parsedSample.rotateClockwise.dump
println("--- Sample Rotated Once And Tilted")
parsedSample.rotateClockwise.tilt.dump
println("--- Sample Rotated Four Times")
parsedSample.rotateClockwise.rotateClockwise.rotateClockwise.rotateClockwise.dump

println("Sample after 1 cycle")
parsedSample.spin.dump

println("Sample after 2 cycles")
parsedSample.spin.spin.dump
  
println("Sample after 3 cycles")
parsedSample.spin.spin.spin.dump
  
println(part2(sample))  
assert(part2(sample) == 64)
println("Sample passes part 2")

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
