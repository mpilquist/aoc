//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-core:2.10.0
import cats.syntax.all.*

enum Direction:
  case N, E, S, W

case class Position(row: Int, col: Int):
  def move(d: Direction): Position =
    d match
      case Direction.N => copy(row = row - 1)
      case Direction.S => copy(row = row + 1)
      case Direction.W => copy(col = col - 1)
      case Direction.E => copy(col = col + 1)

case class Grid(rows: Vector[Vector[Char]]):
  def start: Position =
    val r = rows.indexWhere(_.contains('S'))
    val c = rows(r).indexOf('S')
    Position(r, c)

  def apply(pos: Position): Char =
    val r =
      val s = pos.row % rows.size
      if s < 0 then rows.size + s else s
    val c = 
      val d = pos.col % rows.head.size
      if d < 0 then rows.head.size + d else d
    rows(r)(c)

  def updated(pos: Position, c: Char) = Grid(rows.updated(pos.row, rows(pos.row).updated(pos.col, c)))

  def contains(pos: Position): Boolean =
    pos.row >= 0 && pos.row < rows.size && pos.col >= 0 && pos.col < rows.head.size

  def valid(pos: Position): Boolean = apply(pos) != '#'

  override def toString = rows.map(_.mkString("")).mkString("\n")
 
def render(grid: Grid, positions: Set[Position]): Unit =
  println(positions.foldLeft(grid)((g, p) => g.updated(p, 'O')))

def part1(input: String, steps: Int): Long =
  val grid = Grid(input.linesIterator.map(_.toVector).toVector)

  def step(grid: Grid, positions: Set[Position], steps: Int): Set[Position] =
    if steps == 0 then positions
    else
      val newPositions = positions.flatMap: p =>
        Direction.values.map(p.move).filter(grid.contains).filter(grid.valid)
      step(grid, newPositions, steps - 1)

  val poss = step(grid, Set(grid.start), steps)
  // render(grid, poss)
  poss.size

def part2Step(grid: Grid, positions: Set[Position], steps: Int): Long =
  if steps == 0 then positions.size
  else
    val newPositions = positions.flatMap: p =>
      Direction.values.map(p.move).filter(grid.valid)
    part2Step(grid, newPositions, steps - 1)

def part2Slow(input: String, steps: Int): Long =
  val grid = Grid(input.linesIterator.map(_.toVector).toVector)
  part2Step(grid, Set(grid.start), steps)

def part2(input: String, steps: Int): Long =
  val grid = Grid(input.linesIterator.map(_.toVector).toVector)
  
  // Needed community tip to interpolate a quadratic polynomial at 65, 65 + 131, 65 + 2*131
  val points = List(65, 65 + 131, 65 + 131 + 131).map(x => BigDecimal(x) -> BigDecimal(part2Step(grid, Set(grid.start), x)))
  val x1 = points(0)(0)
  val y1 = points(0)(1)
  val x2 = points(1)(0)
  val y2 = points(1)(1)
  val x3 = points(2)(0)
  val y3 = points(2)(1)

  val a = (x1 * (y3 - y2) + x2 * (y1 - y3) + x3 * (y2 - y1)) / ((x1 - x2) * (x1 - x3) * (x2 - x3))
  val b = (y2 - y1) / (x2 - x1) - a * (x1 + x2)
  val c = y1 - a * (x1 * x1) - b * x1

  def f(x: Long) = (a * x * x + b * x + c).round(java.math.MathContext.UNLIMITED).toLong
  f(steps)

val sample = """...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."""

assert(part1(sample, 6) == 16)
assert(part2Slow(sample, 6) == 16)
assert(part2Slow(sample, 10) == 50)
assert(part2Slow(sample, 50) == 1594)
assert(part2Slow(sample, 100) == 6536)
// assert(part2(sample, 500) == 167004) // This is taking a while
// assert(part2(sample, 1000) == 668697)
// assert(part2(sample, 5000) == 16733044)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

val p1 = part1(input, 64)
println(p1)
assert(p1 == 3853)

val p2 = part2(input, 26501365)
println(p2)
assert(p2 == 639051580070841L)
