package aoc
import scala.util.chaining.*

object day03 extends App:

  val priorityMap: Map[Char, Int] =
    (('a' to 'z') ++ ('A' to 'Z')).zipWithIndex.map((l, r) => (l, r + 1)).toMap

  // Part 1.
  val missMatchPriorities = for {
    rs <- "day03.txt".live
    (l, r) = rs.toCharArray().toList.splitAt(rs.length / 2)
    item <- l.toSet.intersect(r.toSet)
  } yield priorityMap(item)

  missMatchPriorities.sum pipe println

  // Part 2.
  val badgePriorities = for {
    grp <- "day03.txt".live.sliding(3, 3)
    badge <- grp.map(_.toCharArray.toSet).reduce(_.intersect(_))
  } yield priorityMap(badge)

  badgePriorities.sum pipe println
