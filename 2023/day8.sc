//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep co.fs2::fs2-core:3.9.3
//> using dep org.typelevel::spire:0.18.0
import cats.syntax.all.*
import fs2.{Stream, Pull, Pure}


case class DesertMap(instructions: Instructions, network: Map[Node, (Node, Node)])
case class Instructions(value: String)
case class Node(name: String)

def walk(m: DesertMap): Int =
  val Start = Node("AAA")
  val Goal = Node("ZZZ")

  def go(acc: Int, position: Node, directions: Stream[Pure, Boolean]): Pull[Pure, Nothing, Int] =
    if (position == Goal) then Pull.pure(acc)
    else directions.pull.uncons1.flatMap:
      case Some((goLeft, tl)) =>
        val (l, r) = m.network(position)
        go(acc + 1, if goLeft then l else r, tl)
  go(0, Start, Stream.emits(m.instructions.value.toList.map(_ == 'L')).repeat).flatMap(result => Pull.output1(result)).stream.toList.head

def walk1(m: DesertMap, start: Node): Int =
  def go(acc: Int, position: Node, directions: Stream[Pure, Boolean]): Pull[Pure, Nothing, Int] =
    if (position.name.last == 'Z') then Pull.pure(acc)
    else directions.pull.uncons1.flatMap:
      case Some((goLeft, tl)) =>
        val (l, r) = m.network(position)
        go(acc + 1, if goLeft then l else r, tl)
  go(0, start, Stream.emits(m.instructions.value.toList.map(_ == 'L')).repeat).flatMap(result => Pull.output1(result)).stream.toList.head

def walk2(m: DesertMap): Long =
  val perStart = m.network.keySet.toList.filter(_.name.last == 'A').map(s => walk1(m, s))
  import spire.implicits.*
  perStart.foldLeft(1L)(_ lcm _)

def parse(s: String): DesertMap =
  val lines = s.linesIterator.toVector
  val instructions = Instructions(lines.head)
  val network = lines.drop(2).map:
    case (s"$from = ($left, $right)") => Node(from) -> (Node(left) -> Node(right))
  .toMap
  DesertMap(instructions, network)

val sample = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""

println(walk(parse(sample)))

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(walk(parse(input)))

val sample2 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""

println(walk2(parse(sample2)))
println(walk2(parse(input)))
