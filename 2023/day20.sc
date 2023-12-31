//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-core:2.10.0
//> using dep org.typelevel::spire:0.18.0
import cats.syntax.all.*
import spire.implicits.{semigroupOps as _, *}

enum Module:
  case Broadcaster
  case FlipFlop(on: Boolean)
  case Conjunction(inputs: Map[String, Pulse])
  case Output(last: Pulse, counts: Map[Pulse, Long])

case class Wiring(module: Module, destinations: List[String]):
  override def toString = s"""$module -> ${destinations.mkString(", ")}"""


enum Pulse:
  case Low, High

def part1(input: String): Long =
  val modules = parse(input)
  val (finalState, counts) = (0 until 1000).foldLeft((modules, Map.empty[Pulse, Long])):
    case ((state, counts), _) =>
      val (newState, localCounts) = pushButton(state)
      (newState, counts |+| localCounts)

  counts(Pulse.High) * counts(Pulse.Low)

def graphviz(input: String): Unit =
  val modules = parse(input)
  println("copy paste the following and run: pbpaste | dot -Tpng > graph.png && open graph.png")
  println("digraph g {")
  modules.foreach:
    case (name, Wiring(module, destinations)) =>
      val label = module match
        case Module.FlipFlop(_) => "%" + name
        case Module.Conjunction(_) => "&" + name
        case _ => name
      println(s"""  $name [label="$label"]""")
      if destinations.nonEmpty then
        println(s"""  $name -> ${destinations.mkString(", ")}""")
  println("}")


def part2(input: String): Long =
  def out(name: String, state: Map[String, Wiring]) =
    state.getOrElse(name, sys.error("not found " + name)).module.asInstanceOf[Module.Output]

  val modules = parse(input)
  var count = 0L
  var state = modules
  val periodsToCalculate = Set("stout", "tnout", "dtout", "hhout")
  var periods = Map.empty[String, Long]
  while periods.size < periodsToCalculate.size do
    val periodsLeft = periodsToCalculate -- periods.keySet
    periods = periodsLeft.foldLeft(periods):
      (acc, name) =>
        if out(name, state).counts.getOrElse(Pulse.High, 0L) == 1 then acc.updated(name, count) else acc
    val (nextState, _) = pushButton(state)
    state = nextState
    count += 1
  println(periods)
  periods.values.foldLeft(1L)(_ lcm _)

def parse(input: String): Map[String, Wiring] =
  val modules = input.linesIterator
    .map:
      case s"$mod -> $destinations" =>
        val (name, module) =
          if mod.head == '%' then (mod.tail, Module.FlipFlop(false))
          else if mod.head == '&' then (mod.tail, Module.Conjunction(Map.empty))
          else if mod == "broadcaster" then (mod, Module.Broadcaster)
          else sys.error(mod)
        val dests = destinations.split(",").toList.map(_.trim)
        name -> Wiring(module, dests)
    .toMap
  val outputs = 
    val allNames = modules.values.flatMap(_.destinations).toSet
    (allNames -- modules.keySet).map(n => n -> Wiring(Module.Output(Pulse.Low, Map.empty), Nil)).toMap
  modules.foldLeft(modules ++ outputs):
    case (acc, (name, wiring)) =>
      wiring.destinations.foldLeft(acc):
        (acc, d) =>
          acc(d) match
            case Wiring(Module.Conjunction(inputs), d2) => acc.updated(d, Wiring(Module.Conjunction(inputs.updated(name, Pulse.Low)), d2))
            case other => acc

def pushButton(state: Map[String, Wiring]): (Map[String, Wiring], Map[Pulse, Long]) =
  def loop(remaining: Vector[(String, Pulse, String)], state: Map[String, Wiring], counts: Map[Pulse, Long]): (Map[String, Wiring], Map[Pulse, Long]) = 
    if remaining.isEmpty then (state, counts)
    else
      val (from, pulse, to) = remaining.head
      // println(s" + $from --$pulse--> $to")
      val wiring = state(to)
      wiring.module match
        case Module.Broadcaster =>
          loop(
            remaining.tail ++ wiring.destinations.map(d => (to, pulse, d)),
            state,
            counts.updated(pulse, counts.getOrElse(pulse, 0L) + wiring.destinations.size))
        case Module.FlipFlop(on) =>
          if pulse == Pulse.High then loop(remaining.tail, state, counts)
          else
            val output = if on then Pulse.Low else Pulse.High
            loop(
              remaining.tail ++ wiring.destinations.map(d => (to, output, d)),
              state.updated(to, wiring.copy(module = Module.FlipFlop(!on))),
              counts.updated(output, counts.getOrElse(output, 0L) + wiring.destinations.size))
        case Module.Conjunction(inputs) =>
          val newInputs = inputs.updated(from, pulse)
          val output = if newInputs.values.forall(_ == Pulse.High) then Pulse.Low else Pulse.High
          loop(
            remaining.tail ++ wiring.destinations.map(d => (to, output, d)),
            state.updated(to, wiring.copy(module = Module.Conjunction(newInputs))),
            counts.updated(output, counts.getOrElse(output, 0L) + wiring.destinations.size)
          )
        case Module.Output(_, outputCounts) =>
          val newCounts = outputCounts |+| Map(pulse -> 1)
          loop(remaining.tail, state.updated(to, wiring.copy(module = Module.Output(pulse, newCounts))), counts)
    
  loop(Vector(("button", Pulse.Low, "broadcaster")), state, Map(Pulse.Low -> 1))
  
val sample = """broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a"""

val sample2 = """broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output"""

println(part1(sample))
assert(part1(sample) == 32000000L)

println(part1(sample2))
assert(part1(sample2) == 11687500L)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))

graphviz(input)
val p2 = part2(input)
println(p2)
assert(p2 == 233338595643977L)
