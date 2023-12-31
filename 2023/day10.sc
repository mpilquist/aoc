//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1

enum Direction:
  case North, East, South, West

  def toChar: Char = this match
    case North => '↑'
    case East => '→'
    case South => '↓'
    case West => '←'

import Direction.*

case class Position(row: Int, col: Int)

case class Vec(pos: Position, dir: Direction):
  def next: Vec =
    dir match
      case North => copy(pos = Position(pos.row - 1, pos.col))
      case East => copy(pos = Position(pos.row, pos.col + 1))
      case South => copy(pos = Position(pos.row + 1, pos.col))
      case West => copy(pos = Position(pos.row, pos.col - 1))

case class Grid(rows: Array[Array[Char]]):
  lazy val start: Position =
    var row = 0
    var col = -1
    while row < rows.length && col < 0 do
      col = rows(row).indexOf('S')
      row += 1
    Position(row - 1, col)

  def apply(pos: Position): Char =
    rows(pos.row)(pos.col)

  def update(pos: Position, newChar: Char): Unit =
    rows(pos.row)(pos.col) = newChar

  def move(vec: Vec): Vec =
    apply(vec.pos) match
      case '|' => vec.next
      case '-' => vec.next
      case 'L' => vec.copy(dir = if vec.dir == South then East else North).next
      case 'J' => vec.copy(dir = if vec.dir == South then West else North).next
      case '7' => vec.copy(dir = if vec.dir == East then South else West).next
      case 'F' => vec.copy(dir = if vec.dir == West then South else East).next
      case o => sys.error(o.toString)

  def valid(v: Vec): Boolean =
    inBounds(v.pos) && (apply(v.pos) match
      case '|' => v.dir == North || v.dir == South
      case '-' => v.dir == East || v.dir == West
      case 'L' => v.dir == South || v.dir == West
      case 'J' => v.dir == South || v.dir == East
      case '7' => v.dir == East || v.dir == South
      case 'F' => v.dir == West || v.dir == North
      case _ => false)

  def inBounds(pos: Position) =
    pos.row >= 0 && pos.row < rows.length && pos.col >= 0 && pos.col < rows(pos.row).length

  override def toString =
    rows.map(_.mkString("")).mkString("\n")

  def clearPipes(): Unit = 
    rows.mapInPlace(_.mapInPlace:
      case '|' => ' '
      case '-' => ' '
      case 'L' => ' '
      case 'J' => ' '
      case '7' => ' '
      case 'F' => ' '
      case o => o
    )


def part1(map: Grid): Int =
  def go(lvec: Vec, rvec: Vec, acc: Int): Int =
    if lvec.pos == rvec.pos then
      map(lvec.pos) = 'X'
      acc
    else
      def mark(v: Vec): Unit =
        map(v.pos) = v.dir.toChar
      val nl = map.move(lvec)
      val nr = map.move(rvec)
      mark(lvec)
      mark(rvec)
      go(nl, nr, acc + 1)

  val starts = Direction.values.map(d => Vec(map.start, d).next).filter(map.valid)
  assert(starts.size == 2)
  val result = go(starts.head, starts.tail.head, 1)

  map.clearPipes()

  println(map)

  result
     

def part2(map: Grid): Int =

  def mask(c: Char): Char = c match
    case '|' => '!'
    case '-' => '='
    case 'L' => 'l'
    case 'J' => 'j'
    case '7' => '8'
    case 'F' => 'f'
    case o => o

  def unmask(c: Char): Char = c match
    case '!' => '|'
    case '=' => '-'
    case 'l' => 'L'
    case 'j' => 'J'
    case '8' => '7'
    case 'f' => 'F'
    case o => o

  def go(vec: Vec): Unit =
    if map(vec.pos) == 'S' then ()
    else
      val nvec = map.move(vec)
      map(vec.pos) = mask(map(vec.pos))
      go(nvec)

  val starts = Direction.values.map(d => Vec(map.start, d).next).filter(map.valid)
  assert(starts.size == 2)
  go(starts.head)

  map.clearPipes()
  map.rows.mapInPlace(_.mapInPlace(unmask))

  map(map.start) =
    starts.toList.map(_.dir).sortBy(_.ordinal) match
      case List(East, West) => '-'
      case List(North, South) => '|'
      case List(North, East) => 'L'
      case List(East, South) => 'F'
      case List(North, West) => 'J'
      case List(South, West) => '7'
      case other => sys.error(other.toString)


  var result = 0
  var rowIdx = 0
  while rowIdx < map.rows.length do
    val row = map.rows(rowIdx)
    var colIdx = 0
    var in = false
    var lastBend: Char = ' '
    while colIdx < row.length do
      row(colIdx) match
        case '.' | ' ' if in =>
          map(Position(rowIdx, colIdx)) = '$'
          result += 1
        case '|' =>
          in = !in
        case 'F' =>
          lastBend = 'F'
        case '7' =>
          lastBend match
            case 'F' => ()
            case 'L' => in = !in
          lastBend = '7'
        case 'L' =>
          lastBend = 'L'
        case 'J' =>
          lastBend match
            case 'F' => in = !in
            case 'L' => ()
          lastBend = 'J'
        case _ => ()
      colIdx += 1
    rowIdx += 1
  println(map)
  result
  

 
def parse(input: String): Grid =
  Grid(input.linesIterator.map(_.toArray).toArray)

val sample = """-L|F7
7S-7|
L|7||
-L-J|
L|-JF"""

println(part1(parse(sample)))
println(part2(parse(sample)))

println(part2(parse("""...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........""")))

println(part2(parse(""".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...""")))

println(part2(parse("""FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L""")))

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(parse(input)))
println(part2(parse(input)))
