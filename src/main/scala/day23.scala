package aoc
import util.chaining.*
import aoc.board.*

object day23 extends App:
  import Dir.*

  enum Dir(val x: Int, val y: Int):
    case N  extends Dir(0, -1)
    case NE extends Dir(1, -1)
    case E  extends Dir(1, 0)
    case SE extends Dir(1, 1)
    case S  extends Dir(0, 1)
    case SW extends Dir(-1, 1)
    case W  extends Dir(-1, 0)
    case NW extends Dir(-1, -1)

  enum Sweep(val directions: List[Dir], val move: Dir):
    case One extends Sweep(List(N, NW, NE), N)
    case Two extends Sweep(List(S, SE, SW), S)
    case Thr extends Sweep(List(W, NW, SW), W)
    case Fou extends Sweep(List(E, NE, SE), E)

  val sweepSequence = List(Sweep.One, Sweep.Two, Sweep.Thr, Sweep.Fou)

  type Elves = Set[Vec]

  val elves: Elves = "day23.txt".live.board(_.toString).filter(_._2 == "#").keys.toSet

  extension (v: Vec) def +(d: Dir): Vec = Vec(d.x + v.x, d.y + v.y)

  def propose(elves: Elves, sweepSequence: List[Sweep]): Vec => Vec =
    elf =>
      def check(areas: List[Sweep]): Vec =
        areas match
          case hd :: tail =>
            val area = hd.directions.map(d => elf + d)
            if (area.exists(elves)) check(tail)
            else elf + hd.move
          case _ => elf
      check(sweepSequence)

  def shouldMove(elves: Elves): Vec => Boolean = elf =>
    List(N, NE, E, SE, S, SW, W, NW).map(d => elf + d).exists(elves)

  def step(elves: Elves, sm: Vec => Boolean, propose: Vec => Vec): Elves =
    val props: Map[Vec, Set[Vec]] = elves
      .map { case elf =>
        if (sm(elf)) propose(elf) -> elf
        else elf                  -> elf
      }
      .groupMap(_._1)(_._2)

    props.flatMap((k, v) => if (v.size == 1) k :: Nil else v).toSet

  def eval(elves: Elves, sweepSequence: List[Sweep], steps: Int): Elves =
    if (steps == 0) elves
    else
      eval(
        step(elves, shouldMove(elves), propose(elves, sweepSequence)),
        sweepSequence.tail :+ sweepSequence.head,
        steps - 1
      )

  def emptyTiles(elves: Elves): Int =
    (math.abs(elves.map(_.x).max - elves.map(_.x).min) + 1) *
      (math.abs(elves.map(_.y).max - elves.map(_.y).min) + 1) - elves.size

  // Part 1.
  eval(elves, sweepSequence, 10) pipe emptyTiles pipe println

  def eval2(elves: Elves, sweepSequence: List[Sweep], steps: Int): Int =
    if (!elves.exists(shouldMove(elves))) steps
    else
      eval2(
        step(elves, shouldMove(elves), propose(elves, sweepSequence)),
        sweepSequence.tail :+ sweepSequence.head,
        steps + 1
      )

  // Part 2.
  eval2(elves, sweepSequence, 1) pipe println