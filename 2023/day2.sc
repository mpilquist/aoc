//> using scala 3.3.1
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-parse:1.0.0
//> using dep org.typelevel::kittens:3.1.0
import cats.Monoid
import cats.syntax.all.*
import cats.derived.*

case class Cubes(blue: Int, red: Int, green: Int) derives Monoid:
  def power: Int = blue * red * green

case class Game(id: Int, turns: List[Cubes]):
  def possible(actual: Cubes): Boolean =
    turns.forall: t =>
      t.blue <= actual.blue && t.red <= actual.red && t.green <= actual.green

  def minimumNeeded: Cubes =
    turns.foldLeft(Cubes(0, 0, 0))((acc, t) =>
      Cubes(acc.blue max t.blue, acc.red max t.red, acc.green max t.green)
    )  

object Parsing:
  import cats.parse.Rfc5234.*
  import cats.parse.{Parser, Parser0, Numbers}

  private val whitespace = Parser.char(' ').void.rep0
  private def seperator(c: Char) = Parser.char(c).soft.surroundedBy(whitespace).void

  private val cubesParser: Parser[Cubes] =
    val int = Numbers.digits.map(_.toInt)
    val pBlue = (int <* Parser.string(" blue")).map(Cubes(_, 0, 0))
    val pRed = (int <* Parser.string(" red")).map(Cubes(0, _, 0))
    val pGreen = (int <* Parser.string(" green")).map(Cubes(0, 0, _))
    (pBlue.backtrack | pRed.backtrack | pGreen)
      .repSep(seperator(','))
      .surroundedBy(whitespace)
      .map(_.combineAll)

  private val gameParser: Parser[Game] =
    val prefix = Parser.string("Game ") *> Numbers.digits.map(_.toInt) <* Parser.string(": ")
    (prefix ~ cubesParser.repSep0(seperator(';'))
      .surroundedBy(whitespace))
      .map(Game(_, _))

  private val gamesParser: Parser0[List[Game]] =
    gameParser.repSep0(Parser.char('\n').surroundedBy(whitespace))

  def parse(s: String): List[Game] =
    gamesParser.parse(s).fold(e => sys.error(e.toString), (_, games) => games)

end Parsing

def checksum(input: String, actual: Cubes): Int =
  Parsing.parse(input).filter(_.possible(actual)).foldMap(_.id)

def powerMin(input: String) =
  Parsing.parse(input).map(_.minimumNeeded.power).sum

val exampleInput = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

println(checksum(exampleInput, Cubes(14, 12, 13)))
println(powerMin(exampleInput))

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))
 
println(checksum(input, Cubes(14, 12, 13)))
println(powerMin(input))
