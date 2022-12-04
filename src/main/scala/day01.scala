package aoc

import util.chaining.*

object day01 extends App:

  val carry: List[List[Int]] = "day01.txt".live
    .map(_.toIntOption)
    .foldLeft((List.empty[List[Int]], List.empty[Int])) {
      case ((result, running), None)        => (running :: result, Nil)
      case ((result, running), Some(value)) => (result, value :: running)
    }
    ._1

  // One
  carry.map(_.sum).max pipe println

  // Two
  carry.map(_.sum).sorted(Ordering.Int.reverse).take(3).sum pipe println
