//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-core:2.10.0
import cats.syntax.all.*

enum Result:
  case Rejected
  case Accepted
  case Workflow(name: String)

type Attribute = Char

enum Operation:
  case Gt, Lt

enum Rule:
  case Rejected
  case Accepted
  case Condition(attribute: Attribute, operation: Operation, rhs: Long, result: Result)
  case Workflow(name: String)

case class Workflow(name: String, rules: List[Rule])

case class Part(attributes: Map[Attribute, Int])

def parseWorkflows(lines: Vector[String]): Map[String, Workflow] =

  def parseResult(s: String): Result = s match
    case "R" => Result.Rejected
    case "A" => Result.Accepted
    case other => Result.Workflow(other)

  val workflows = lines.takeWhile(_.nonEmpty).map:
    case s"$name{$rules}" =>
      val rs = rules.split(",").toList.map:
        case "R" => Rule.Rejected
        case "A" => Rule.Accepted
        case s"$attr<$value:$to" =>
          Rule.Condition(attr.head, Operation.Lt, value.toLong, parseResult(to))
        case s"$attr>$value:$to" =>
          Rule.Condition(attr.head, Operation.Gt, value.toLong, parseResult(to))
        case other => 
          Rule.Workflow(other)
      Workflow(name, rs)

  workflows.map(w => w.name -> w).toMap

def part1(input: String): Long =
  val lines = input.linesIterator.toVector
  val workflowByName = parseWorkflows(lines)

  val parts = lines.dropWhile(_.nonEmpty).tail.map:
    case s"{x=$x,m=$m,a=$a,s=$s}" => Part(Map('x' -> x.toInt, 'm' -> m.toInt, 'a' -> a.toInt, 's' -> s.toInt))

  def evalRule(rule: Rule, part: Part): Option[Result] =
    rule match
      case Rule.Rejected => Some(Result.Rejected)
      case Rule.Accepted => Some(Result.Accepted)
      case Rule.Condition(attr, op, rhs, result) =>
        val predicate = if op == Operation.Lt then (_: Int) < rhs else (_: Int) > rhs
        if predicate(part.attributes(attr)) then Some(result) else None
      case Rule.Workflow(name) => Some(Result.Workflow(name))

  def evalWorkflow(workflow: Workflow, part: Part): Result =
    workflow.rules.collectFirstSome(r => evalRule(r, part)).get // Last rule always matches everything so get is safe

  val result = parts.foldMap: part =>
    def loop(result: Result): Boolean =
      result match
        case Result.Rejected => false
        case Result.Accepted => true
        case Result.Workflow(w) => loop(evalWorkflow(workflowByName(w), part))
    if loop(Result.Workflow("in")) then part.attributes.values.sum else 0

  result

def part2(input: String): Long =
  val lines = input.linesIterator.toVector
  val workflowByName = parseWorkflows(lines)

  case class Interval(start: Long, stop: Long):
    def size: Long = stop - start

    def intersect(that: Interval): Interval =
      Interval(start max that.start, stop min that.stop)

    override def toString = s"[$start, $stop)"

  object Interval:
    def empty: Interval = Interval(0, 0)

  case class Parts(attributes: Map[Attribute, Interval]):
    def size: Long = attributes.values.map(_.size).product
    def -(that: Parts): Parts =
      def complement(a: Interval, b: Interval): Interval =
        if a == b then a else if a.start == b.start then Interval(b.stop, a.stop) else Interval(a.start, b.start)
      if this == that then Parts.Empty
      else Parts(attributes.map((k, v) => k -> complement(v, that.attributes(k))))
    override def toString = 
      attributes.toList
        .sortBy(_(0))
        .map(t => s"${t(0)}: ${t(1)}")
        .mkString("Parts(", ", ", ")")
      
  object Parts:
    def All = Parts("xmas".map(_ -> Interval(1, 4001)).toMap)
    def Empty = Parts("xmas".map(_ -> Interval.empty).toMap)

  def evalRule(rule: Rule, parts: Parts): (Parts, Result) =
    rule match
      case Rule.Rejected => (parts, Result.Rejected)
      case Rule.Accepted => (parts, Result.Accepted)
      case Rule.Workflow(w) => (parts, Result.Workflow(w))
      case Rule.Condition(attr, op, rhs, result) =>
        val ruleInterval = op match
          case Operation.Lt => Interval(1, rhs)
          case Operation.Gt => Interval((rhs + 1), 4001)
        val updatedParts = Parts(parts.attributes.updated(attr, (parts.attributes(attr) intersect ruleInterval)))
        (updatedParts, result)

  def accepted(remaining: List[(Workflow, Parts)], acc: List[Parts]): List[Parts] =
    remaining match
      case Nil => acc
      case (workflow, parts) :: tl =>
        // println(s" - evaluating ${workflow.name} with $parts")
        val (partsAfterRules, newRemaining, newAcc) =
          workflow.rules.foldLeft((parts, tl, acc)):
            case ((parts, tl, acc), rule) =>
              val (matchedParts, result) = evalRule(rule, parts)
              val out = result match
                case Result.Accepted => (tl, matchedParts :: acc)
                case Result.Rejected => (tl, acc)
                case Result.Workflow(name) => ((workflowByName(name) -> matchedParts) :: tl, acc)
              (parts - matchedParts) *: out
        assert(partsAfterRules.size == 0) // Last rule always matches the entire input
        // if newRemaining != tl then println(s"    - enqueued ${newRemaining.drop(tl.size).map(_(0).name)}")
        // if newAcc != acc then println(s"    - accepted ${newAcc.take(a2.size - acc.size)}")
        accepted(newRemaining, newAcc)

  accepted(List(workflowByName("in") -> Parts.All), Nil).foldMap(_.size)


val sample = """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"""

println(part1(sample))
assert(part1(sample) == 19114)
assert(part2(sample) == 167409079868000L)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
