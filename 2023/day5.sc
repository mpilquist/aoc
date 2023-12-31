//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-parse:1.0.0
import cats.data.NonEmptyList
import cats.syntax.all.*

case class Almanac(seeds: List[Long], mappings: List[Mapping]):
  def lowestLocation: Long =
    seeds.foldLeft(Long.MaxValue):
      (acc, seed) =>
        val loc = mappings.foldLeft(seed)((x, mapping) => mapping(x))
        if loc < acc then loc else acc

  def part2: Long =
    val (_, allSeeds) =
      seeds.foldLeft((None: Option[Long], List.empty[Range])):
        case ((None, acc), s) => (Some(s), acc)
        case ((Some(start), acc), len) => (None, Range(start, start, len) :: acc)

    mappings
      .foldLeft(allSeeds):
        (xs, mapping) => xs.flatMap(mapping(_)).map(r => r.copy(sourceStart = r.destStart))
      .map(_.destStart)
      .min

case class Mapping(ranges: List[Range]):
  def apply(source: Long): Long =
    ranges.collectFirstSome(_.lookup(source)).getOrElse(source)

  def apply(source: Range): List[Range] =
    val sorted = ranges.flatMap(r => r.lookup(source)).sortBy(_.sourceStart)
    val result = if sorted.isEmpty then List(source)
    else
      val pfx = 
        if sorted.head.sourceStart == source.sourceStart then Nil
        else List(Range(source.destStart, source.sourceStart, sorted.head.sourceStart - source.sourceStart))
      val delta = source.destStart - source.sourceStart
      val mid = sorted.foldLeft(pfx):
        case (acc, range) =>
          if acc.isEmpty then range :: acc
          else
            if acc.head.sourceEnd == range.sourceStart then range :: acc
            else
              val gap = Range(acc.head.sourceEnd + delta, acc.head.sourceEnd, range.sourceStart - acc.head.sourceEnd)
              range :: gap :: acc
      val rev = if mid.head.sourceEnd == source.sourceEnd then mid else Range(mid.head.sourceEnd + delta, mid.head.sourceEnd, source.sourceEnd - mid.head.sourceEnd) :: mid
      rev.reverse
    result

case class Range(destStart: Long, sourceStart: Long, length: Long):
  def sourceEnd = sourceStart + length
  
  def lookup(source: Long): Option[Long] =
    if source >= sourceStart && source < sourceEnd
    then Some(destStart + source - sourceStart) else None

  def lookup(that: Range): Option[Range] =
    if sourceEnd < that.sourceStart || that.sourceEnd < sourceStart then None
    else if sourceStart <= that.sourceStart then
      if sourceEnd <= that.sourceEnd then
        // x------x
        //    y--------y
        Some(Range(destStart + that.sourceStart - sourceStart, that.sourceStart, sourceEnd - that.sourceStart)).filter(_.length > 0)
      else
        // x------------x
        //    y----y
        Some(Range(destStart + that.sourceStart - sourceStart, that.sourceStart, that.length))
    else // sourceStart > that.sourceStart
      if that.sourceEnd <= sourceEnd then
        // y-----y
        //    x-----x
        Some(Range(destStart, sourceStart, that.sourceEnd - sourceStart)).filter(_.length > 0)
      else
        // y--------y
        //    x---x
        Some(Range(destStart, sourceStart, length))

object Parsing:
  import cats.parse.Rfc5234.*
  import cats.parse.{Parser, Parser0, Numbers}

  private val sps = sp.rep0
  private val long = Numbers.digits.map(_.toLong)
  private val nl = cr | lf | crlf

  private def seedsParser: Parser0[List[Long]] =
    Parser.string("seeds: ") *> long.repSep0(sp)

  private def mappingParser: Parser[Mapping] =
    val name = (alpha | Parser.char('-')).rep
    val header = name *> Parser.string(" map:")
    val range = long.repSep(3, 3, sp).map:
      case NonEmptyList(dest, src :: len :: Nil) => Range(dest, src, len)
      case _ => sys.error("impossible")
    header *> nl *> range.repSep(nl).map(rs => Mapping(rs.toList)) <* nl.?

  private val almanacParser: Parser0[Almanac] =
    ((seedsParser <* nl.rep) ~ mappingParser.repSep0(nl)).map:
      case (s, m) => Almanac(s, m)

  def parse(s: String): Almanac =
    almanacParser.parse(s).fold(e => sys.error(e.toString), (_, res) => res)

end Parsing

val sample = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

val sampleAlmanac = Parsing.parse(sample)
println(sampleAlmanac.lowestLocation)
println(sampleAlmanac.part2)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

val inputAlmanac = Parsing.parse(input)
println(inputAlmanac.lowestLocation)
println(inputAlmanac.part2)

