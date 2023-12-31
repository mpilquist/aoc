//> using scala 3.3.1
//> using toolkit 0.2.1

val Pattern = """([0-9]|zero|one|two|three|four|five|six|seven|eight|nine)""".r

def valueOfDigit(s: String): Int =
  s match
    case c if c.head.isDigit => c.head - '0'
    case "one" => 1
    case "two" => 2
    case "three" => 3
    case "four" => 4
    case "five" => 5
    case "six" => 6
    case "seven" => 7
    case "eight" => 8
    case "nine" => 9
    case "zero" => 0


def calibrateLinePart1(line: String): Int =
  val firstIndex = line.indexWhere(_.isDigit)
  val lastIndex = line.lastIndexWhere(_.isDigit)
  val combined = line(firstIndex).toString + line(lastIndex).toString
  combined.toInt

def calibrationPart1(text: String): Int =
  text.linesIterator.map(calibrateLinePart1).sum

def calibrateLine(line: String): Int =
  val first = Pattern.findFirstIn(line).get
  var start = line.length()
  var last: Option[String] = None
  while (last.isEmpty)
    start = start - 1
    last = Pattern.findFirstIn(line.substring(start))
  val digits = valueOfDigit(first).toString + valueOfDigit(last.get).toString
  digits.toInt

def calibration(text: String): Int =
  text.linesIterator.map(calibrateLine).sum

val part1Sample = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

assert(calibrationPart1(part1Sample) == 142)
assert(calibration(part1Sample) == 142)

assert(calibration("""two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
""") == 281)


val input = os.read(os.pwd / "input" / scriptPath.replace(".sc", ".txt"))
  
println(calibrationPart1(input))
println(calibration(input))
