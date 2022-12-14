package aoc
import util.chaining.*
import scala.math.Ordered.given

object day13 extends App:

  enum Packet:
    case PS(v: Seq[Packet])
    case PI(v: Int)

  object Packet:
    def fromString(s: String): Packet =
      s match
        case s"[$data]" => PS(parseSeq(data))
        case _          => throw NotImplementedError()

    def chewNumber(v: String): (String, String) =
      v.span(_.isDigit)

    def chewBlock(v: String): (String, String) =
      def helper(remain: String, taken: String, depth: Int): (String, String) =
        if remain.isEmpty() then (taken, "")
        else
          remain.head match
            case '['               => helper(remain.tail, taken + '[', depth + 1)
            case ']' if depth == 1 => (taken, remain.tail)
            case ']'               => helper(remain.tail, taken + ']', depth - 1)
            case _                 => helper(remain.tail, taken + remain.head, depth)
      helper(v.tail, "", 1)

    def parseSeq(s: String): Vector[Packet] =
      if (s.isEmpty()) Vector.empty
      else if (s.head == '[')
        val (block, remain) = chewBlock(s)
        PS(parseSeq(block)) +: parseSeq(remain)
      else if (s.head == ',') parseSeq(s.tail)
      else
        val (block, remain) = chewNumber(s)
        PI(block.toInt) +: parseSeq(remain)

  val lines: List[String] = "day13.txt".live

  given Ordering[Packet] = new {
    def compare(l: Packet, r: Packet): Int = (l, r) match
      case (Packet.PI(vl), Packet.PI(vr)) => vl.compare(vr)
      case (lv: Packet.PI, rv: Packet.PS) => compare(Packet.PS(Seq(lv)), rv)
      case (lv: Packet.PS, rv: Packet.PI) => compare(lv, Packet.PS(Seq(rv)))
      case (Packet.PS(lv), Packet.PS(rv)) =>
        lv.zip(rv).map(compare).find(_ != 0).getOrElse(lv.length.compare(rv.length))
  }

  // Part 1.
  lines
    .filter(_ != "")
    .map(Packet.fromString)
    .sliding(2, 2)
    .map(l => l.head -> l.tail.head)
    .map((l, r) => l < r)
    .zipWithIndex
    .map((a, b) => (a, b + 1))
    .filter((a, _) => a)
    .map((_, b) => b)
    .sum pipe println

  // Part 2.
  ("[[2]]" :: "[[6]]" :: lines)
    .filter(_ != "")
    .map(Packet.fromString)
    .sorted
    .zipWithIndex
    .map((a, b) => (a, b + 1))
    .filter((a, b) => a == Packet.fromString("[[2]]") || a == Packet.fromString("[[6]]"))
    .map((a, b) => b)
    .product pipe println
