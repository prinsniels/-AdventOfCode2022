package aoc
import scala.util.chaining.*
import aoc.board.*

object day08 extends App:

  val board = "day08.txt".live.board(x => x.toInt)

  val up    = Vec(0, 1)
  val down  = Vec(0, -1)
  val left  = Vec(-1, 0)
  val right = Vec(1, 0)

  def visible(b: Board[Int], dir: Vec): Vec => Boolean =
    def step(s: Vec, mh: Int): Boolean =
      b.get(s) match
        case None              => true
        case Some(v) if v < mh => true && step(s + dir, mh)
        case Some(v)           => false
    start => step(start + dir, b(start))

  def trees(b: Board[Int], dir: Vec): Vec => Int =
    def step(s: Vec, mh: Int): Int =
      b.get(s) match
        case None              => 0
        case Some(v) if v < mh => 1 + step(s + dir, mh)
        case Some(v)           => 1
    start => step(start + dir, b(start))

  val visibleCount: Vec => Boolean = point =>
    visible(board, up)(point) || visible(board, down)(point) ||
      visible(board, left)(point) || visible(board, right)(point)
  
  val mostScenic: Vec => Int = point =>
    trees(board, up)(point) * trees(board, down)(point) *
      trees(board, left)(point) * trees(board, right)(point)
  
  // Part 1:
  board.keysIterator.map(visibleCount).count(x => x) pipe println

  // Part 2:
  board.keysIterator.map(mostScenic).max pipe println