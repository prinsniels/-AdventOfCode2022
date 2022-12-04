package aoc
import scala.util.chaining.*

object day04 extends App:

  def parse(row: String): (Set[Int], Set[Int]) =
    row match
      case s"$a-$b,$c-$d" =>
        (a.toInt to b.toInt).toSet -> (c.toInt to d.toInt).toSet

  def completeOverlap(l: Set[Int], r: Set[Int]): Boolean =
    l.subsetOf(r) || r.subsetOf(l)

  def overlap(l: Set[Int], r: Set[Int]): Boolean =
    !l.intersect(r).isEmpty

  // Part 1.
  "day04.txt".live.map(parse).count(completeOverlap) pipe println
  // Part 2.
  "day04.txt".live.map(parse).count(overlap) pipe println
