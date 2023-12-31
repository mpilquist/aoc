//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-core:2.10.0
import cats.syntax.all.*

enum Direction:
  case N, E, S, W

  def vertical: Boolean = this match
    case N | S => true
    case E | W => false

  def toArrow: Char = this match
    case N => '^'
    case S => 'V'
    case E => '>'
    case W => '<'

case class Position(row: Int, col: Int)

case class Vec(pos: Position, dir: Direction):
  def next: Vec =
    dir match
      case Direction.N => copy(pos = pos.copy(row = pos.row - 1))
      case Direction.S => copy(pos = pos.copy(row = pos.row + 1))
      case Direction.W => copy(pos = pos.copy(col = pos.col - 1))
      case Direction.E => copy(pos = pos.copy(col = pos.col + 1))

case class Grid(rows: Vector[Vector[Char]]):
  def apply(pos: Position): Char =
    rows(pos.row)(pos.col)

  def updated(pos: Position, char: Char): Grid =
    Grid(rows.updated(pos.row, rows(pos.row).updated(pos.col, char)))

  def contains(pos: Position): Boolean =
    pos.row >= 0 && pos.row < rows.size && pos.col >= 0 && pos.col < rows.head.size

  def merge(that: Grid): Grid =
    Grid(Vector.tabulate(rows.size, rows.head.size): (r, c) =>
      val p = Position(r, c)
      val l = apply(p)
      if l != ' ' then l else that(p))

  override def toString =
    rows.map(_.mkString("")).mkString("\n")

case class Traversal(height: Int, width: Int, visited: Set[Vec]):
  def toGrid: Grid =
    val emptyGrid = Grid(Vector.fill(height)(Vector.fill(width)(' ')))
    visited.foldLeft(emptyGrid): (grid, v) =>
      val c = grid(v.pos) match
        case ' ' => v.dir.toArrow
        case c if c.isDigit => (c.toInt + 1).toChar
        case _ => '2'
      grid.updated(v.pos, c)

  def energized: Int =
    visited.map(_.pos).size
    
  override def toString = toGrid.toString


def trace(grid: Grid, visited: Set[Vec], toVisit: List[Vec]): Traversal =
  toVisit match
    case Nil => Traversal(grid.rows.size, grid.rows.head.size, visited)
    case ray :: rest =>
      if visited.contains(ray) then trace(grid, visited, rest)
      else if grid.contains(ray.pos) then
        inline def recurse(more: Vec*): Traversal =
          trace(grid, visited + ray, more.toList ::: rest)
        grid(ray.pos) match
          case '.' => recurse(ray.next)
          case '|' =>
            if ray.dir.vertical then recurse(ray.next)
            else recurse(ray.copy(dir = Direction.N).next, ray.copy(dir = Direction.S).next)
          case '-' =>
            if !ray.dir.vertical then recurse(ray.next)
            else recurse(ray.copy(dir = Direction.E).next, ray.copy(dir = Direction.W).next)
          case '/' =>
            val newDir = ray.dir match
              case Direction.N => Direction.E
              case Direction.S => Direction.W
              case Direction.W => Direction.S
              case Direction.E => Direction.N
            recurse(ray.copy(dir = newDir).next)
          case '\\' =>
            val newDir = ray.dir match
              case Direction.N => Direction.W
              case Direction.S => Direction.E
              case Direction.W => Direction.N
              case Direction.E => Direction.S
            recurse(ray.copy(dir = newDir).next)
      else trace(grid, visited, rest)

def parse(input: String): Grid =
  Grid(input.linesIterator.map(_.toVector).toVector)

def part1(input: String): Int =
  val grid = parse(input)
  val traversed = trace(grid, Set.empty, List(Vec(Position(0, 0), Direction.E)))
  traversed.energized

def part2(input: String): Int =
  val grid = parse(input)
  val height = grid.rows.size
  val width = grid.rows.head.size
  val max =
    val verticals = (0 until width).flatMap(i => Vector(Vec(Position(0, i), Direction.S), Vec(Position(height - 1, i), Direction.N)))
    val horizontals = (0 until height).flatMap(i => Vector(Vec(Position(i, 0), Direction.E), Vec(Position(i, width - 1), Direction.W)))
    (verticals ++ horizontals).map(v => trace(grid, Set.empty, List(v)).energized).max
  max

val sample = """.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."""

assert(part1(sample) == 46)
assert(part2(sample) == 51)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
