//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
import scala.collection.immutable.SortedSet

// Used some hints from https://github.com/merlinorg/aoc2023/blob/main/src/main/scala/Day17FP.scala

enum Direction:
  case N, E, S, W

  def toArrow: Char = this match
    case N => '^'
    case S => 'V'
    case E => '>'
    case W => '<'

  def turnLeft: Direction = this match
    case N => W
    case E => N
    case S => E
    case W => S

  def turnRight: Direction = this match
    case N => E
    case E => S
    case S => W
    case W => N

case class Position(row: Int, col: Int)

case class Vec(pos: Position, dir: Direction):
  def next: Vec =
    dir match
      case Direction.N => copy(pos = pos.copy(row = pos.row - 1))
      case Direction.S => copy(pos = pos.copy(row = pos.row + 1))
      case Direction.W => copy(pos = pos.copy(col = pos.col - 1))
      case Direction.E => copy(pos = pos.copy(col = pos.col + 1))

case class Grid(rows: Vector[Vector[Int]]):
  val bottomRight: Position = Position(rows.size - 1, rows.head.size - 1)

  def apply(pos: Position): Int =
    rows(pos.row)(pos.col)

  def contains(pos: Position): Boolean =
    pos.row >= 0 && pos.row < rows.size && pos.col >= 0 && pos.col < rows.head.size

case class Move(vec: Vec, count: Int)

given Ordering[Move] = Ordering.by(m => (m.vec.pos.row, m.vec.pos.col, m.vec.dir.toArrow, m.count))

def trace(grid: Grid, visited: Set[Move], remaining: SortedSet[(Int, Move)], min: Int, max: Int): Int =
  if remaining.isEmpty then sys.error("no solution")
  else
    val (loss, m) = remaining.head
    val rest = remaining.tail
    if m.vec.pos == grid.bottomRight && m.count >= min then loss
    else
      val possibleMoves = List(
          m.copy(vec = m.vec.next, count = m.count + 1),
          m.copy(vec = m.vec.copy(dir = m.vec.dir.turnLeft).next, count = 1),
          m.copy(vec = m.vec.copy(dir = m.vec.dir.turnRight).next, count = 1),
        ).filter(n => n.count <= max && (n.vec.dir == m.vec.dir || m.count >= min) && !visited.contains(n) && grid.contains(n.vec.pos))
      val possibleMovesWithLoss = possibleMoves.map(m => (loss + grid(m.vec.pos)) -> m)
      trace(grid, visited ++ possibleMoves, rest ++ possibleMovesWithLoss, min, max)
        

def part1(input: String): Int =
  val grid = parse(input)
  trace(grid, Set.empty, SortedSet((0, Move(Vec(Position(0, 0), Direction.E), 1))), 0, 3)

def part2(input: String): Int =
  val grid = parse(input)
  // Check both starting directions as otherwise the minimum constraint will rule out going south
  trace(grid, Set.empty, SortedSet((0, Move(Vec(Position(0, 0), Direction.E), 1)), (0, Move(Vec(Position(0, 0), Direction.S), 1))), 4, 10)

def parse(input: String): Grid =
  Grid(input.linesIterator.map(_.toVector.map(_ - '0')).toVector) 

val sample = """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"""

assert(part1(sample) == 102)
assert(part2(sample) == 94)

val sample2 = """111111111111
999999999991
999999999991
999999999991
999999999991"""

assert(part2(sample2) == 71)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
