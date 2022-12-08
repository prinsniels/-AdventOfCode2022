package aoc
import scala.util.chaining.*

object tmp extends App:

  enum StdOutLine:
    case Cd(v: String)
    case Dir(v: String)
    case File(v: String, size: Int)
    case Ls

  object StdOutLine:
    def apply(v: String): StdOutLine =
      v match
        case s"$$ cd $a" => Cd(a)
        case s"$$ ls"    => Ls
        case s"dir $a"   => Dir(a)
        case s"$a $b"    => File(b, a.toInt)

  enum DiskObject:
    case File(name: String, size: Int)
    case Dir(path: List[String])

  val diskStruct = "day07.txt".live
    .map(x => StdOutLine(x))
    .foldLeft((List.empty[String], List.empty[(DiskObject.Dir, DiskObject)])) {
      case ((path, collection), out) =>
        out match
          case StdOutLine.Cd(v) if v == "/"  => (List("/"), collection)
          case StdOutLine.Cd(v) if v == ".." => (path.tail, collection)
          case StdOutLine.Cd(v)              => (v :: path, collection)
          case StdOutLine.Dir(v)        => (path, (DiskObject.Dir(path), DiskObject.Dir(v :: path)) :: collection)
          case StdOutLine.File(v, size) => (path, (DiskObject.Dir(path), DiskObject.File(v, size)) :: collection)
          case StdOutLine.Ls            => (path, collection)

    }
    ._2
    .groupBy(_._1)
    .mapValues(x => x.map(_._2))

  val size: DiskObject => Int = d =>
    d match
      case DiskObject.File(v, s) => s
      case DiskObject.Dir(p)     => diskStruct.getOrElse(DiskObject.Dir(p), List.empty).map(size(_)).sum

  val folderSizes: Map[DiskObject.Dir, Int] = diskStruct.keysIterator.map(k => k -> diskStruct(k).map(size).sum).toMap

  // Part 1.
  folderSizes.values.filter(_ <= 100_000).sum pipe println

  // Part 2.
  folderSizes.values.filter(_ >= 30000000 - (70000000 - (folderSizes(DiskObject.Dir(List("/")))))).min pipe println