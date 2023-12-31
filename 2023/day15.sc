//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-core:2.10.0
import cats.syntax.all.*

def part1(input: String): Int =
  val instructions = input.replace("\n", "").replace("\r", "").split(',').toVector
  instructions.foldMap(hash)

def hash(s: String): Int =
  s.foldLeft(0)((h, c) => (h + c.toInt) * 17 % 256)

enum Operation:
  case Remove
  case Focus(length: Int)

case class Instruction(label: String, operation: Operation)
case class Focus(label: String, length: Int)

def part2(input: String): Int =
  val instructions =
    input
      .replace("\n", "")
      .replace("\r", "")
      .split(',')
      .toVector
      .map:
        case s"${label}=${length}" => Instruction(label, Operation.Focus(length.toInt))
        case s"${label}-" => Instruction(label, Operation.Remove)
          
  val config = instructions.foldLeft(Vector.fill(256)(Vector.empty[Focus])):
    (config, inst) =>
      val boxId = hash(inst.label)
      val box = config(boxId)
      val newBox = inst.operation match
        case Operation.Remove => box.filterNot(_.label == inst.label)
        case Operation.Focus(l) =>
          val existing = box.indexWhere(_.label == inst.label)
          if existing < 0 then
            box :+ Focus(inst.label, l)
          else box.updated(existing, Focus(inst.label, l))
      config.updated(boxId, newBox)

  config.zipWithIndex.foldMap:
    (box, idx) =>
      box.zipWithIndex.foldMap:
        (f, lidx) =>
          (idx + 1) * f.length * (lidx + 1)

val sample = """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""

assert(part1(sample) == 1320)
assert(part2(sample) == 145)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
