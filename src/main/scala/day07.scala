package aoc
import scala.util.chaining.*

object day07 extends App:

  type Path = Vector[String]

  trait Tree:
    val name: String
    val size: Int

  case class Folder(name: String, children: List[Tree]) extends Tree:
    val size = children.map(_.size).sum

  case class File(name: String, size: Int) extends Tree

  extension (fld: Folder)
    def push(path: Path, f: File): Folder =
      path match
        case hd +: tail =>
          fld.children.find(_.name == hd) match
            case None => fld.copy(children = Folder(hd, List.empty).push(tail, f) :: fld.children)
            case Some(v: Folder) =>
              fld.copy(children = v.push(tail, f) :: fld.children.filterNot(_.name == hd))
            case Some(_) => throw NotImplementedError()
        case _ => fld.copy(children = f :: fld.children)

    def filterSum(pred: Int => Boolean): Int =
      (if pred(fld.size) then fld.size else 0) + fld.children
        .collect { case f: Folder => f }
        .map(_.filterSum(pred))
        .sum

    def doGet(pred: Int => Boolean): List[Folder] =
      if pred(fld.size) then
        fld :: fld.children.collect { case f: Folder => f }.flatMap(_.doGet(pred))
      else fld.children.collect { case f: Folder => f }.flatMap(_.doGet(pred))

  val fPattern = raw"(\d+) (\D+)".r
  val root     = Folder("", List.empty)

  lazy val fs = "day07.txt".live
    .foldLeft(Vector.empty[String], root) { case ((path, root), line) =>
      line match
        case s"$$ cd $a" =>
          if a == ".." then (path.dropRight(1), root)
          else (path :+ a, root)
        case fPattern(s, n) => (path, root.push(path, File(n, s.toInt)))
        case _              => (path, root)
    }
    ._2

  // Part 1.
  fs.filterSum(_ <= 100000) pipe println

  // Part 2.
  fs.doGet(_ >= 30000000 - (70000000 - fs.size)).map(_.size).min pipe println
