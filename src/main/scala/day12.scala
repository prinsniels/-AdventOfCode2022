package aoc
import board.*
import util.chaining.*

object day12 extends App:

  val board = "day12.txt".live.board(x => x)

  val up    = Vec(0, 1)
  val down  = Vec(0, -1)
  val left  = Vec(-1, 0)
  val right = Vec(1, 0)

  val scores = ('a' to 'z').zipWithIndex.toMap + ('S' -> 0) + ('E' -> 25)

  // This builds a function that can be seen as a Graph. A graph is brought
  // back to a function that takes a `A` and returns a List[A] (A => List[A])
  // in our case Vec => List[Vec] representing the the locations we can goto from A
  def g(b: Board[Char], s: Map[Char, Int]): Vec => List[Vec] = v =>
    val curHeight = s(b(v))
    List(up, down, left, right)
      .map(dir => v + dir)
      .filter(b.keySet)
      .filter(nV => (s(b(nV)) - curHeight) <= 1)

  val start = board.find((v, c) => c == 'S').map((v, _) => v).get
  val end   = board.find((v, c) => c == 'E').map((v, _) => v).get

  // utility function for searching the graph
  val search: Vec => Map[Vec, Vec] = v => breathFirst(g(board, scores), v)(x => ()).toMap

  // Part 1.
  unwind(search(start), end).size pipe println

  // Part 2.
  // Check the path for all `a`
  board
    .filter((v, c) => c == 'a')
    .map((a, _) => unwind(search(a), end).size)
    .filter(_ > 0)
    .min pipe println
