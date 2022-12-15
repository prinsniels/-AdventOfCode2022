package aoc

import util.chaining.*
import board.*

object day15 extends App:

  def manhattan(l: Vec, r: Vec): Int =
    math.abs(l.x - r.x) + math.abs(l.y - r.y)

  def spawn(start: Vec, end: Vec, dir: Vec): LazyList[Vec] =
    LazyList.iterate(start)(v => v + dir).takeWhile(_ != end)

  case class ScanResult(sensor: Vec, beacon: Vec):
    lazy val dist: Int = manhattan(sensor, beacon)

  extension (s: ScanResult)
    def atY(y: Int): Set[Vec] = {
      LazyList
        .iterate(Vec(s.sensor.x, y))(_ + Vec(-1, 0))
        .takeWhile(s.covers)
        ++ LazyList
          .iterate(Vec(s.sensor.x, y))(_ + Vec(1, 0))
          .takeWhile(s.covers)
    }.toSet

    def surrounding: LazyList[Vec] =
      val left   = s.sensor + Vec(-s.dist - 1, 0)
      val top    = s.sensor + Vec(0, s.dist + 1)
      val right  = s.sensor + Vec(s.dist + 1, 0)
      val bottom = s.sensor + Vec(0, -s.dist - 1)

      spawn(left, bottom, Vec(1, -1)) ++
        spawn(bottom, right, Vec(1, 1)) ++
        spawn(right, top, Vec(-1, 1)) ++
        spawn(top, left, Vec(-1, -1))

    def covers(v: Vec): Boolean =
      manhattan(v, s.sensor) <= s.dist

  object ScanResult:
    def fromString(s: String): ScanResult =
      s match
        case s"Sensor at x=$x, y=$y: closest beacon is at x=$bx, y=$by" =>
          ScanResult(
            Vec(x.toInt, y.toInt),
            Vec(bx.toInt, by.toInt)
          )

  def inCoversAScanResult(es: List[ScanResult])(v: Vec): Boolean =
    es.exists(e => e.covers(v))

  val scans                         = "day15.txt".live.map(ScanResult.fromString)
  val devices                       = scans.flatMap(e => Set(e.beacon, e.sensor)).toSet
  val coveredByScan: Vec => Boolean = inCoversAScanResult(scans)

  // Part 1.
  scans.flatMap(_.atY(2000000)).filterNot(devices).toSet.size pipe println

  // Part 2.
  scans
    .map(_.surrounding) // flatMap will create the complete list, used map reduce
    .reduce(_ ++ _)
    .filter(v => v.x >= 0 && v.x <= 4_000_000 && v.y >= 0 && v.y <= 4_000_000)
    .filterNot(coveredByScan)
    .map(v => v.x.toLong * 4_000_000 + v.y)
    .head pipe println
