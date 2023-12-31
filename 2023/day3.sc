//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using lib org.typelevel::cats-core:2.10.0
import cats.Monoid
import cats.syntax.all.*

// Extracts the part number at the specified position, mutating the grid so the part number is no longer present
def extract(grid: Array[Array[Char]], rowIdx: Int, colIdx: Int): Option[Int] =
  if rowIdx < 0 || rowIdx >= grid.length then None
  else
    val row = grid(rowIdx)
    if colIdx < 0 || colIdx >= row.length then None
    else
      val c = row(colIdx)
      if c.isDigit then
        var start = colIdx
        var end = colIdx
        while start > 0 && row(start - 1).isDigit do start = start - 1
        while end < row.length && row(end).isDigit do end = end + 1
        val result = new String(row.slice(start, end)).toInt 
        (start until end).foreach(i => row(i) = '.')
        Some(result)
      else None

// Traverse the grid looking for symbols; for each symbol, extract all adjacent parts
// The input grid is copied b/c extraction works on a mutable scratch space
def parts(grid: Array[Array[Char]]): List[Int] =
  val scratch = grid.map(_.clone)
  val bldr = List.newBuilder[Int]
  var rowIdx = 0
  while rowIdx < scratch.length do
    val row = scratch(rowIdx)
    var colIdx = 0
    while colIdx < row.length do
      val c = row(colIdx)
      val isSymbol = !(c.isDigit || c == '.')
      if isSymbol then
        val dirs = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
        dirs.foreach: (rDelta, cDelta) => 
          extract(scratch, rowIdx + rDelta, colIdx + cDelta).foreach(n => bldr += n)
      colIdx += 1
    rowIdx += 1
  bldr.result()

def partsBySymbol[A: Monoid](grid: Array[Array[Char]], f: (Char, List[Int]) => Option[A]): A =
  val scratch = grid.map(_.clone)
  var result = Monoid[A].empty
  var rowIdx = 0
  while rowIdx < scratch.length do
    val row = scratch(rowIdx)
    var colIdx = 0
    while colIdx < row.length do
      val c = row(colIdx)
      val isSymbol = !(c.isDigit || c == '.')
      if isSymbol then
        val dirs = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
        val partNums = dirs.flatMap: (rDelta, cDelta) =>
          extract(scratch, rowIdx + rDelta, colIdx + cDelta)
        result = f(c, partNums).fold(result)(_ |+| result)
      colIdx += 1
    rowIdx += 1
  result

def sumOfGearRatios(grid: Array[Array[Char]]): Int =
  partsBySymbol(grid, (c, parts) => if c == '*' && parts.size == 2 then Some(parts.head * parts.tail.head) else None)

// Reimplementation of part 1 using the partsBySymbol function
def sumOfPartsUsingPart2(grid: Array[Array[Char]]): Int =
  partsBySymbol(grid, (_, parts) => Some(parts.sum))

def parse(s: String): Array[Array[Char]] =
  s.linesIterator.map(line => line.toArray).toArray
  
def sumOfPartNumbers(s: String): Int =
  parts(parse(s)).sum

def sumOfGearRatios(s: String): Int =
  sumOfGearRatios(parse(s))

val sample = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

println(sumOfPartNumbers(sample))
assert(sumOfPartNumbers(sample) == 4361)
assert(sumOfPartsUsingPart2(parse(sample)) == 4361)

println(sumOfGearRatios(sample))
assert(sumOfGearRatios(sample) == 467835)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(sumOfPartNumbers(input))
println(sumOfGearRatios(input))
