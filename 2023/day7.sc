//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1
//> using dep org.typelevel::cats-parse:1.0.0
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.math.Ordered

enum Type:
  case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

case class Hand(cards: List[Int]) extends Ordered[Hand]:
  private val groups = cards.groupBy(x => x)

  val `type`: Int =
    import Type.*
    val tpe = groups.size match
      case 1 => FiveOfAKind
      case 2 => if groups.values.exists(_.size == 4) then FourOfAKind else FullHouse
      case 3 => if groups.values.exists(_.size == 3) then ThreeOfAKind else TwoPair
      case 4 => OnePair
      case 5 => HighCard
    tpe.ordinal

  def compare(that: Hand): Int =
    if this.`type` < that.`type` then -1
    else if this.`type` > that.`type` then 1
    else cards.zip(that.cards).find(_ != _).map(_.compare(_)).getOrElse(0)

type Bid = Int

def part1(input: List[(Hand, Bid)]) =
  input.sortBy(_._1).zipWithIndex.foldLeft(0):
    case (acc, ((_, bid), idx)) => acc + (bid * (idx + 1))

def cardToStrength(c: Char) = c match
  case 'A' => 14
  case 'K' => 13
  case 'Q' => 12
  case 'J' => 11
  case 'T' => 10
  case i if i >= '2' && i <= '9' => i - '0' 
  case _ => sys.error("invalid card")

def parse(input: String): List[(Hand, Bid)] =
  input.linesIterator.toList.map:
    case (s"$hand $bid") => 
      Hand(hand.map(cardToStrength).toList) -> bid.toInt

val sample = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

println(part1(parse(sample)))

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(parse(input)))

