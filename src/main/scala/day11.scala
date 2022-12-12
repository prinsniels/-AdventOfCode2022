package aoc
import util.chaining.*

object day11 extends App:

  case class Monkey(
      items: Vector[Long],
      scans: Int,
      opp: Long => Long,
      div: Int,
      throwTrue: Int,
      throwFalse: Int
  )

  object Monkey:
    val pattern =
      raw"items: ([\d, ]+)  \D+ new = (.*)  Test:\D+(\d+)\D+(\d+)\D+(\d+)".r

    def opp(s: String): Long => Long =
      in =>
        s match
          case s"$left $o $right" =>
            val l = if (left == "old") in else left.toLong
            val r = if (right == "old") in else right.toLong

            if (o == "*") l * r else l + r

    def fromString(s: String): Monkey =
      pattern.findFirstMatchIn(s) match
        case None => throw Exception("Not even supposed to be here today")
        case Some(grps) =>
          Monkey(
            items = grps.group(1).split(",").map(_.trim.toLong).toVector,
            scans = 0,
            div = grps.group(3).toInt,
            throwTrue = grps.group(4).toInt,
            throwFalse = grps.group(5).toInt,
            opp = opp(grps.group(2))
          )

  def lcm(a: Long, b: Long): Long =
    a * b / gcd(a, b)

  def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  def run(monkeys: Map[Int, Monkey], rounds: Int, reductionRate: Int): Map[Int, Monkey] = 
    val troopSize = monkeys.keys.size
    /*The idea is that the numers get big and we want to counter that
    The decission of the next monkey is based on the division of the current value
    So when we take the modules of the value based on the least common denominator we keep a
    value that is manageble, but without influencing the decission taken
     */
    val mod = monkeys.values.map(_.div.toLong).reduce(lcm)
    (0 until rounds).foldLeft(monkeys) { case (roundMonkeys, round) => 
            (0 until troopSize).foldLeft(roundMonkeys) { case (_monkeys, id) =>
                    val monkey = _monkeys(id)
                    val scansThisRound = monkey.items.size

                    val nwState = monkey.items.foldLeft(_monkeys){ case (mks, item) => 
                        val nxtWorryLvl = (monkey.opp(item) / reductionRate) % mod
                        val nxtMonkeyId = if(nxtWorryLvl % monkey.div == 0) monkey.throwTrue else monkey.throwFalse
                        val nxtMonkey = mks(nxtMonkeyId)
                        mks + (nxtMonkeyId -> nxtMonkey.copy(items = nxtMonkey.items :+ nxtWorryLvl))
                    } + (id -> monkey.copy(items = Vector(), scans = monkey.scans + scansThisRound))   

                    nwState 
                 }
         }

  val monkeys = "day11.txt".live
        .grouped(7)
        .map(_.mkString)
        .map(Monkey.fromString)
        .zipWithIndex
        .map(_.swap)
        .toMap

  // Part 1.
  run(monkeys, 20, 3).values.map(_.scans).toList.sorted.takeRight(2).product pipe println
  // Part 2.
  run(monkeys, 10_000, 1).values.map(_.scans.toLong).toList.sorted.takeRight(2).product pipe println
