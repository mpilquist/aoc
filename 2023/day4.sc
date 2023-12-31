//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-parse:1.0.0
import cats.syntax.all.*

case class Card(id: Int, winning: Set[Int], have: Set[Int]):
  val matching: Int = winning.intersect(have).size
  val score: Int =
    if matching == 0 then 0 else (1 << (matching- 1))

def totalScore(cards: List[Card]): Int =
  cards.foldMap(_.score)

def part2Slow(cards: List[Card]): Int =
  val cardsById = cards.map(c => c.id -> c).toMap
  
  def loop(remaining: List[Card], processed: List[Card]): List[Card] =
    remaining match
      case Nil => processed
      case hd :: tl =>
        val won = ((hd.id + 1) until (hd.id + 1 + hd.matching)).map(cardsById).toList
        loop(won ++ tl, hd :: processed)

  loop(cards, Nil).size

def part2Optimized(cards: List[Card]): Int =
  def loop(remaining: List[(Card, Int)], count: Int): Int =
    remaining match
      case Nil => count
      case (c, cnt) :: tl =>    
        val (pfx, sfx) = tl.splitAt(c.matching)
        val newPfx = pfx.map:
          case (c, n) => (c, n + cnt)
        loop(newPfx ++ sfx, count + cnt)

  loop(cards.map(c => c -> 1), 0)

object Parsing:
  import cats.parse.Rfc5234.*
  import cats.parse.{Parser, Parser0, Numbers}

  private val whitespace = Parser.char(' ').void.rep0
  private def seperator(c: Char) = Parser.char(c).soft.surroundedBy(whitespace).void

  private def cardParser: Parser[Card] =
    val int = Numbers.digits.map(_.toInt)
    val prefix = Parser.string("Card") *> whitespace *> int <* Parser.string(": ")
    val winners = int.repSep(whitespace).surroundedBy(whitespace)
    val have = Parser.char('|').surroundedBy(whitespace) *> int.repSep(whitespace).surroundedBy(whitespace)
    (prefix ~ winners ~ have).map: 
      case ((i, w), h) => Card(i, w.toList.toSet, h.toList.toSet)

  private val cardsParser: Parser0[List[Card]] =
    cardParser.repSep0(Parser.char('\n').surroundedBy(whitespace))

  def parse(s: String): List[Card] =
    cardsParser.parse(s).fold(e => sys.error(e.toString), (_, cards) => cards)

end Parsing

val sample = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

val sampleCards = Parsing.parse(sample)
assert(totalScore(sampleCards) == 13)
assert(part2Slow(sampleCards) == 30)
assert(part2Optimized(sampleCards) == 30)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

val inputCards = Parsing.parse(input)
println(totalScore(inputCards))
println(part2Slow(inputCards))
println(part2Optimized(inputCards))

