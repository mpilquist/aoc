//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1

def part1(input: String): Long =
  compute(input, 2)

def part2(input: String): Long =
  compute(input, 1000000)

def compute(input: String, scale: Long): Long =
  val galaxies = parse(input, scale)
  galaxies.combinations(2).map(pts => pts(0).distanceTo(pts(1))).sum

case class Point(x: Long, y: Long):
  def distanceTo(other: Point): Long =
    (x - other.x).abs + (y - other.y).abs

def parse(input: String, scale: Long): IndexedSeq[Point] =
  val grid = input.linesIterator.map(line => line.toVector).toVector
  val height = grid.length
  val width = grid.head.length

  val horizontalLookup =
    val result = Array.ofDim[Long](width)
    var i = 0
    var x = 0L
    while i < width do
      result(i) = x
      x += (if grid.forall(_(i) == '.') then scale else 1)
      i += 1
    result

  val verticalLookup =
    val result = Array.ofDim[Long](height)
    var i = 0
    var y = 0L
    while i < height do
      result(i) = y
      y += (if grid(i).forall(_ == '.') then scale else 1)
      i += 1
    result

  for
    i <- 0 until height
    j <- 0 until width
    if grid(i)(j) != '.'
  yield Point(verticalLookup(i), horizontalLookup(j))


val sample = """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""

assert(part1(sample) == 374)
assert(compute(sample, 10) == 1030)
assert(compute(sample, 100) == 8410)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
