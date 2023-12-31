//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-core:2.10.0
import cats.syntax.all.*

def part1(input: String): Int =
  val patterns = parse(input)
  patterns.map(summarize).sum

def parse(input: String): Vector[Vector[Vector[Char]]] =
  val (acc, pattern) = 
    input.linesIterator.foldLeft((Vector.empty[Vector[Vector[Char]]], Vector.empty[Vector[Char]])):
      case ((acc, pattern), line) =>
        if line.isEmpty then (acc :+ pattern, Vector.empty)
        else (acc, pattern :+ line.toVector)
  acc :+ pattern

def summarize(pattern: Vector[Vector[Char]]): Int =
  findReflection(pattern).map(_ * 100).orElse(
    findReflection(pattern.transpose)).getOrElse(0)

def findReflection(pattern: Vector[Vector[Char]]): Option[Int] =
  (1 until pattern.size)
    .find: i =>
      val (pfx, sfx) = pattern.splitAt(i)
      pfx.reverse.zip(sfx).forall(_ == _)


def part2(input: String): Int =
  val patterns = parse(input)
  patterns.map(summarize2).sum

def summarize2(pattern: Vector[Vector[Char]]): Int =
  findReflection2(pattern).map(_ * 100).orElse(
    findReflection2(pattern.transpose)).getOrElse(0)

def differences(l: Vector[Char], r: Vector[Char]): Int =
  l.zip(r).filter(t => t(0) != t(1)).size

def findReflection2(pattern: Vector[Vector[Char]]): Option[Int] =
  (1 until pattern.size)
    .find: i =>
      val (pfx, sfx) = pattern.splitAt(i)
      pfx.reverse.zip(sfx).map(differences).sum == 1

val sample = """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""

assert(part1(sample) == 405)
assert(part2(sample) == 400)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
