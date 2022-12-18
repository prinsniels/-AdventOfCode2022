package aoc
import util.chaining.*
import scala.collection.immutable.Queue
import aoc.board.*

type Graph[A] = A => List[A]


def breathFirst[A](graph: Graph[A], root: A)(drawHook: Set[A] => Unit): List[(A, A)] =
  def step(frontier: Queue[A], visited: Set[A], acc: List[(A, A)]): List[(A, A)] =
    drawHook(visited)
    frontier.dequeueOption match
      case None => acc
      case Some(hd, tail) =>
        val options = graph(hd).filterNot(visited)
        step(tail.appendedAll(options), visited ++ options, options.map(o => o -> hd) ++ acc)
  step(Queue(root), Set(root), List.empty)

def depthFirst[A](graph: Graph[A], root: A)(drawHook: Set[A] => Unit): List[(A, A)] =
  def step(frontier: List[A], visited: Set[A], acc: List[(A, A)]): List[(A, A)] =
    drawHook(visited)
    frontier match
      case hd :: tail =>
        val options = graph(hd).filterNot(visited)
        step(options ++ tail, visited ++ options, options.map(o => o -> hd) ++ acc)
      case _ => acc
  step(List(root), Set(root), List.empty)


def unwind(g: Map[Vec, Vec], end: Vec): List[Vec] =
  g.get(end) match
    case None => Nil
    case Some(parent) => end :: unwind(g, parent) 
  