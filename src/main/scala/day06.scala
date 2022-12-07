package aoc
import scala.util.chaining.*

object day06 extends App:

  val calc: Int => Int = sz =>
    "day06.txt".live
      .take(1)
      .head
      .sliding(sz, 1)
      .zipWithIndex
      .dropWhile(_._1.toSet.size != sz)
      .map(_._2 + sz)
      .next()

  // Part 1.
  calc(4) pipe println
  // Part 2.
  calc(14) pipe println
