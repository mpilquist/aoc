//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-core:2.10.0
import cats.syntax.all.*

// Needed a hint from https://github.com/merlinorg/aoc2023/blob/main/src/main/scala/Day18.scala

enum Direction:
  case N, E, S, W

case class Position(row: Long, col: Long):
  def move(dir: Direction, distance: Long): Position = dir match
    case Direction.N => Position(row - distance, col)
    case Direction.S => Position(row + distance, col)
    case Direction.E => Position(row, col + distance)
    case Direction.W => Position(row, col - distance)

case class Instruction(dir: Direction, meters: Long)

def areaOfPolygon(vertices: Vector[Position]): Long =
  ((vertices.zipWithIndex.foldMap:
    case (v, i) => 
      val u = vertices(if i < vertices.size - 1 then i + 1 else 0)
      v.row * u.col - u.row * v.col
    ) / 2).abs

def areaOfDig(instructions: List[Instruction]): Long =
  val vertices = instructions
    .foldLeft(Vector(Position(0, 0))):
      case (poss, Instruction(dir, meters)) =>
        poss :+ poss.last.move(dir, meters)
    .init

  val internalArea = ((vertices.zipWithIndex.foldMap:
    case (v, i) => 
      val u = vertices(if i < vertices.size - 1 then i + 1 else 0)
      v.row * u.col - u.row * v.col
    ) / 2).abs

  val perimeter = instructions.foldMap(_.meters)

  internalArea + (perimeter / 2) + 1

def part1(input: String): Long =
  val instructions = input.linesIterator
    .map:
      case s"$d $m (#$c)" =>
        val dir = d match
          case "U" => Direction.N
          case "R" => Direction.E
          case "D" => Direction.S
          case "L" => Direction.W
        Instruction(dir, m.toInt)
    .toList

  areaOfDig(instructions)

 def part2(input: String): Long =
  val instructions = input.linesIterator
    .map:
      case s"$d $m (#$c)" =>
        val meters = java.lang.Long.parseLong(c.take(5), 16)
        val dir = c.drop(5) match
          case "0" => Direction.E
          case "1" => Direction.S
          case "2" => Direction.W
          case "3" => Direction.N
        Instruction(dir, meters)
    .toList

  areaOfDig(instructions)
     

val sample = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"""

assert(part1(sample) == 62)
assert(part2(sample) == 952408144115L)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
