package aoc
import util.chaining.*
import scala.annotation.tailrec

object day19 extends App:

  enum Robot:
    case Ore, Clay, Obs, Geo

  case class Stock(ore: Int, clay: Int, obsidian: Int, geo: Int)

  opaque type BluePrint = Map[Robot, Stock]

  object BluePrint:
    def fromString(s: String): BluePrint = s match
      case s"Blueprint $a: Each ore robot costs $b ore. Each clay robot costs $c ore. Each obsidian robot costs $d ore and $e clay. Each geode robot costs $f ore and $g obsidian." =>
        Map(
          Robot.Ore  -> Stock(b.toInt, 0, 0, 0),
          Robot.Clay -> Stock(c.toInt, 0, 0, 0),
          Robot.Obs  -> Stock(d.toInt, e.toInt, 0, 0),
          Robot.Geo  -> Stock(f.toInt, 0, g.toInt, 0)
        )

    def gain: BluePrint = Map(
      Robot.Ore  -> Stock(1, 0, 0, 0),
      Robot.Clay -> Stock(0, 1, 0, 0),
      Robot.Obs  -> Stock(0, 0, 1, 0),
      Robot.Geo  -> Stock(0, 0, 0, 1)
    )

  extension (stock: Stock)
    def +(o: Stock): Stock =
      Stock(stock.ore + o.ore, stock.clay + o.clay, stock.obsidian + o.obsidian, stock.geo + o.geo)

    def -(o: Stock): Stock =
      Stock(stock.ore - o.ore, stock.clay - o.clay, stock.obsidian - o.obsidian, stock.geo - o.geo)

    def *(i: Int): Stock =
      Stock(stock.ore * i, stock.clay * i, stock.obsidian * i, stock.geo * i)

    def |>(o: Stock): Boolean =
      stock.ore >= o.ore && stock.clay >= o.clay && stock.obsidian >= o.obsidian && stock.geo >= o.geo

  case class State(steps: Int, st: Stock, robots: Map[Robot, Int])

  extension (state: State)
    def step: State = state.copy(steps = state.steps + 1)

    def gather: State =
      state.copy(st = state.st + state.robots.map((r, v) => BluePrint.gain(r) * v).reduce(_ + _))

    def take(s: Stock): State =
      state.copy(st = state.st - s)

    def addRobot(r: Robot): State =
      state.copy(robots = state.robots + (r -> (state.robots.getOrElse(r, 0) + 1)))

  def toGraph(bp: BluePrint): State => List[State] = state =>
    val nw = state.step.gather
    // favour doing something over doing nothing by adding nw to then end, this really matters...
    bp.filter((r, s) => state.st |> s).map((r, c) => nw.addRobot(r).take(c)).toList :+ nw

  def dfs(g: State => List[State], pf: (State, State) => Boolean, root: State): State =
    @tailrec
    def solve(frontier: List[State], visited: Set[State], best: State): State =
      frontier match
        case Nil => best
        case head :: next =>
          val options = g(head).filterNot(visited).filterNot(nxt => pf(nxt, best))
          solve(options ++ next, visited ++ options, if (best.st.geo > head.st.geo) best else head)
    solve(List(root), Set.empty, root)

  def prune(maxTurns: Int, bluePrint: BluePrint): (State, State) => Boolean =
    val maxOre  = bluePrint.values.map(_.ore).max
    val maxClay = bluePrint.values.map(_.clay).max
    val maxObs  = bluePrint.values.map(_.obsidian).max

    (nxt, best) =>
      // don't run more then max turns
      (nxt.steps > maxTurns) ||
        // don't create more robots than we can consume in one turn
        nxt.robots(Robot.Ore) > maxOre ||
        nxt.robots(Robot.Clay) > maxClay ||
        nxt.robots(Robot.Obs) > maxObs ||
        // Cut it off when we never going to reach out current max
        (nxt.st.geo + (nxt.robots(
          Robot.Geo
        ) * (maxTurns - nxt.steps)) + (maxTurns - nxt.steps to 0 by -1).sum) <= best.st.geo

  val defaultState: State = State(
    steps = 0,
    st = Stock(0, 0, 0, 0),
    robots = Map(Robot.Ore -> 1, Robot.Clay -> 0, Robot.Obs -> 0, Robot.Geo -> 0)
  )

  val bps = "day19.txt".live.map(BluePrint.fromString).zipWithIndex

  // Part 1.
  bps.map { case (bp, idx) =>
    (dfs(toGraph(bp), prune(24, bp), defaultState).st.geo * (idx + 1)) tap { s =>
      println((idx + 1, s))
    }
  }.sum pipe println

  // Part 2.
  bps
    .take(3)
    .map { case (bp, idx) =>
      dfs(toGraph(bp), prune(32, bp), defaultState).st.geo tap { s => println((idx + 1, s)) }
    }
    .product pipe println
