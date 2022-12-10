package aoc
import scala.util.chaining.*

object day10 extends App:

  enum Signal:
    case Noop
    case Addx(v: Int)

  object Signal:
    def fromString(s: String): Signal =
      s match
        case s"noop"    => Noop
        case s"addx $v" => Addx(v.toInt)

  val signals = "day10.txt".live.map(Signal.fromString)

  val cpu = signals
    .foldLeft(List(1)) { case (mem, sig) =>
      sig match
        case Signal.Noop    => mem.head :: mem
        case Signal.Addx(v) => (mem.head + v) :: mem.head :: mem // it takes 2 cycles to take effect
    }
    .reverse

  // Part 1.
  // We are interested in the X during a cycle, we calculated at the end of
  // each cycle, so wee need to add 1 at the beginning of the list to reflect
  // during each cycle or increase zipWithIndex by 1 or lower all interesting Indexes by 1
  val interestingIndexes = Set(20, 60, 100, 140, 180, 220)
  (1 :: cpu).zipWithIndex
    .filter(x => interestingIndexes(x._2))
    .map((x, y) => x * y)
    .sum pipe println

  def draw(sc: Int, p: Int): String =
    if (math.abs(sc - p) <= 1) "#" else "."

  // Part 2.
  cpu.grouped(40).foreach(_.zipWithIndex.map((sc, p) => draw(sc, p)).mkString pipe println)
