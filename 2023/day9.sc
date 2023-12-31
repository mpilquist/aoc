//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep co.fs2::fs2-core:3.9.3
//> using dep org.typelevel::spire:0.18.0
import cats.syntax.all.*


def part1(input: Vector[Vector[Int]]): Int =
  input.map(interpolate).sum

def interpolate(input: Vector[Int]): Int =
  val deltas = input.zip(input.tail).map:
    case (cur, next) => next - cur
  if deltas.forall(_ == 0) then input.last
  else interpolate(deltas) + input.last

def part2(input: Vector[Vector[Int]]): Int =
  input.map(previous).sum

def previous(input: Vector[Int]): Int =
  val deltas = input.zip(input.tail).map:
    case (cur, next) => next - cur
  if deltas.forall(_ == 0) then input.head
  else input.head - previous(deltas)

def parse(input: String): Vector[Vector[Int]] =
  input.linesIterator.map(_.split(" ").map(_.toInt).toVector).toVector

val sample = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

println(part1(parse(sample)))
println(part2(parse(sample)))

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(parse(input)))
println(part2(parse(input)))
