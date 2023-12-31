//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-parse:1.0.0
import cats.data.NonEmptyList
import cats.syntax.all.*

case class Record(time: Long, distance: Long):
  def waysToWin: Long = 
    val minWinner = (1L until time).find(t => (time - t) * t > distance).get
    val maxWinner = (time until 1 by -1).find(t => (time - t) * t > distance).get
    maxWinner - minWinner + 1

object Parsing:
  import cats.parse.Rfc5234.*
  import cats.parse.{Parser, Parser0, Numbers}

  private val sps = sp.rep0
  private val long = Numbers.digits.map(_.toLong)
  private val nl = cr | lf | crlf

  private val recordsParser: Parser0[List[Record]] =
    val timesParser = Parser.string("Time:") *> sps *> (long.repSep0(sp.rep) <* sps)
    val distancesParser = Parser.string("Distance:") *> sps *> (long.repSep0(sp.rep) <* sps)
    ((timesParser <* nl) ~ distancesParser).map(_ zip _).map(_.map(Record(_, _)))
    
  def parse(s: String): List[Record] =
    recordsParser.parse(s).fold(e => sys.error(e.toString), (_, res) => res)

  def parsePart2(s: String): Record =
    val spacedDigits = Numbers.digits.repSep0(sp.rep).map(_.mkString("").toLong)
    val timeParser = Parser.string("Time:") *> sps *> (spacedDigits <* sps)
    val distanceParser = Parser.string("Distance:") *> sps *> (spacedDigits <* sps)
    val parser = ((timeParser <* nl) ~ distanceParser).map(Record(_, _))
    parser.parse(s).fold(e => sys.error(e.toString), (_, res) => res)


end Parsing

val sample = """Time:      7  15   30
Distance:  9  40  200"""

val sampleRecords = Parsing.parse(sample)
println(sampleRecords.map(_.waysToWin).foldLeft(1L)(_ * _))

val part2Sample = Parsing.parsePart2(sample)
println(part2Sample.waysToWin)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

val records = Parsing.parse(input)
println(records.map(_.waysToWin).foldLeft(1L)(_ * _))

val part2Input = Parsing.parsePart2(input)
println(part2Input.waysToWin)
