//> using scala 3.3.1
//> using option -deprecation
//> using toolkit 0.2.1

def part1(input: String): Long =
  input.linesIterator.map(arrangements).sum

def part2(input: String): Long =
  input.linesIterator.map(arrangements2).sum

def arrangements(line: String): Long =
  val (springs, countsStr) = line.splitAt(line.indexOf(' '))
  val counts = countsStr.split(",").map(_.trim.toInt).toList
  expand(springs.toList, counts, None)

def arrangements2(line: String): Long =
  val (springs, countsStr) = line.splitAt(line.indexOf(' '))
  val counts = countsStr.split(",").map(_.trim.toInt).toList
  val replicatedSprings = List.fill(5)(springs).mkString("", "?", "")
  val replicatedCounts = List.fill(5)(counts).flatten
  expand(replicatedSprings.toList, replicatedCounts, None)

val memo = collection.mutable.Map[(List[Char], List[Int], Option[Int]), Long]()

def expand(remaining: List[Char], counts: List[Int], remBroken: Option[Int]): Long =
  memo.getOrElseUpdate((remaining, counts, remBroken), 
    remaining match
      case Nil =>
        if counts.isEmpty && remBroken.getOrElse(0) == 0 then 1
        else 0
      case hd :: tl =>
        hd match
          case '.' =>
            if remBroken == None || remBroken == Some(0) then expand(tl, counts, None)
            else 0
          case '#' =>
            remBroken match
              case Some(b) =>
                if b > 0 then expand(tl, counts, Some(b - 1))
                else 0
              case None =>
                if counts.isEmpty then 0
                else expand(tl, counts.tail, Some(counts.head - 1))
          case '?' =>
            remBroken match
              case Some(b) =>
                if b > 0 then expand(tl, counts, Some(b - 1))
                else expand(tl, counts, None)
              case None => 
                if counts.isEmpty then expand(tl, counts, None)
                else expand(tl, counts.tail, Some(counts.head - 1)) + expand(tl, counts, None)
  )

assert(arrangements("???.### 1,1,3") == 1)
assert(arrangements(".??..??...?## 1,1,3") == 4)
assert(arrangements("?###???????? 3,2,1") == 10)
  
val sample = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""

assert(part1(sample) == 21)

assert(arrangements2("???.### 1,1,3") == 1)
assert(arrangements2(".??..??...?## 1,1,3") == 16384)
assert(arrangements2("?###???????? 3,2,1") == 506250)
assert(part2(sample) == 525152)

val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))

println(part1(input))
println(part2(input))
