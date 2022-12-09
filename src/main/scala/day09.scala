package aoc
import scala.util.chaining.*

object day09 extends App:

  case class Vec(x: Int, y: Int)

  enum Move(val x: Int, val y: Int):
    case Up    extends Move(0, 1)
    case Down  extends Move(0, -1)
    case Left  extends Move(-1, 0)
    case Right extends Move(1, 0)

  object Move:
    def fromText(s: String): List[Move] =
      s match
        case s"U $a" => List.fill(a.toInt)(Up)
        case s"D $a" => List.fill(a.toInt)(Down)
        case s"L $a" => List.fill(a.toInt)(Left)
        case s"R $a" => List.fill(a.toInt)(Right)

  extension (v: Vec)
    def |>(m: Move): Vec = Vec(v.x + m.x, v.y + m.y)
    def +(o: Vec): Vec   = Vec(v.x + o.x, v.y + o.y)
    def connects(o: Vec): Boolean =
      val dx = v.x - o.x
      val dy = v.y - o.y
      dx <= 1 && dx >= -1 && dy <= 1 && dy >= -1

  def follow(t: Vec, h: Vec): Vec =
    if t.connects(h) then t.copy()
    else {
      val dx = h.x - t.x
      val dy = h.y - t.y
      Vec(
        if dx < 0 then -1 else if dx > 0 then 1 else 0,
        if dy < 0 then -1 else if dy > 0 then 1 else 0
      ) + t
    }

  def moveRope(m: Move, rope: List[Vec]): List[Vec] =
    def run(rm: List[Vec], prev: Vec): List[Vec] =
      rm match
        case hd :: tail =>
          val nT = follow(hd, prev)
          nT :: run(tail, nT)
        case Nil => Nil

    val nH = rope.head |> m
    nH :: run(rope.tail, nH)

  def simulate(ropeSize: Int, moves: List[Move]): (List[Vec], Set[Vec]) =
    moves.foldLeft(List.fill(ropeSize)(Vec(0, 0)), Set.empty[Vec]) { case ((ks, t), m) =>
      val nRope = moveRope(m, ks)
      (nRope, t + nRope.last)
    }

  val moves = "day09.txt".live
    .flatMap(Move.fromText)

  // Part 1.
  simulate(2, moves)._2.size pipe println
  // Part 2.
  simulate(10, moves)._2.size pipe println
