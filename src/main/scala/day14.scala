package aoc
import util.chaining.*
import board.*

object day14 extends App:

  def parsePath(s: String): List[Vec] =
    s.split(" -> ")
      .map { vs =>
        vs match
          case s"$x,$y" => Vec(x.toInt, y.toInt)
      }
      .toList

  def constructPath(rp: List[Vec]): Board[Char] =
    rp.sliding(2, 1).flatMap(grp => part(grp.head, grp.tail.head)).map(v => v -> '#').toMap

  val direction: (Int, Int) => Int =
    (l, r) => if ((l - r) > 0) -1 else 1

  def part(l: Vec, r: Vec): List[Vec] =
    if (l.x == r.x) (l.y to r.y by direction(l.y, r.y)).map(y => Vec(l.x, y)).toList
    else (l.x to r.x by direction(l.x, r.x)).map(x => Vec(x, l.y)).toList

  // options to try, `down`, `downLeft`, `downRight`
  val steps = List(Vec(0, 1), Vec(-1, 1), Vec(1, 1))

  def nextPos(options: List[Vec])(cur: Vec, board: Board[Char]): Option[Vec] =
    options.map(o => cur + o).filterNot(o => Set('#', 'O')(board.getOrElse(o, '.'))).headOption

  val next: (Vec, Board[Char]) => Option[Vec] = nextPos(steps)

  def drop(start: Vec, board: Board[Char], path: List[Vec], drops: Int): Option[List[Vec]] =
    // keep dropping until None is reached
    next(start, board) match
      case None => Some(start :: path)
      case Some(value) if drops < 100 =>
        drop(value, board, start :: path, if (start.x == value.x) drops + 1 else 0)
      case Some(value) => None

  def simulate(b: Board[Char], start: Vec, count: Int): Int =
    drop(start, b, List.empty, 0) match
      case None                                     => { b.show; count }
      case Some(value) if value.head == Vec(500, 0) => count + 1
      case Some(value) => simulate(b + (value.head -> 'O'), start, count + 1)

  val caveMap = "day14.txt".live.map(parsePath).map(constructPath).reduce(_ ++ _)

  simulate(caveMap, Vec(500, 0), 0) pipe println

  val withFloor = ((caveMap.minX - 10000) to (caveMap.maxX + 10000))
    .map(x => Vec(x, caveMap.maxY + 2) -> '#')
    .toMap ++ caveMap

  simulate(withFloor, Vec(500, 0), 0) pipe println
