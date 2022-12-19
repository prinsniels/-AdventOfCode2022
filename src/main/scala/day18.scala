package aoc
import util.chaining.*
import aoc.board.Vec

object day18 extends App:

  case class Point(x: Int, y: Int, z: Int)

  object Point:
    def fromString(s: String): Point =
      s match
        case s"$x,$y,$z" => Point(x.toInt, y.toInt, z.toInt)

  extension (p: Point)
    def +(o: Point): Point =
      Point(p.x + o.x, p.y + o.y, p.z + o.z)

  val pointCloud = "day18.txt".live.map(Point.fromString).toSet

  val options = List(
    Point(0, 0, 1),
    Point(0, 0, -1),
    Point(0, 1, 0),
    Point(0, -1, 0),
    Point(1, 0, 0),
    Point(-1, 0, 0)
  )

  def countCreator(options: List[Point], counts: Point => Boolean): Point => Int = p =>
    options.map(_ + p).count(counts)

  // Part 1.
  pointCloud.toList.map(countCreator(options, p => !pointCloud(p))).sum pipe println

  // Part 2.
  // Define a bounding box around the droplet
  val minX = pointCloud.map(_.x).min - 1
  val minY = pointCloud.map(_.y).min - 1
  val minZ = pointCloud.map(_.z).min - 1
  val maxX = pointCloud.map(_.x).max + 1
  val maxY = pointCloud.map(_.y).max + 1
  val maxZ = pointCloud.map(_.z).max + 1

  def toGraph(options: List[Point], pointCloud: Set[Point]): Point => List[Point] =
    p =>
      options
        .map(o => o + p)
        .filterNot(pointCloud)
        .filterNot(o =>
          o.x < minX || o.y < minY || o.z < minZ || o.x > maxX || o.y > maxY || o.z > maxZ
        )

  // When bfs can reach the point, than it is a surface area
  val reachable: Set[Point] =
    breathFirst(toGraph(options, pointCloud), Point(minX, minY, minZ))(identity)
      .map((l, r) => l)
      .toSet

  pointCloud.toList.map(countCreator(options, reachable)).sum pipe println
