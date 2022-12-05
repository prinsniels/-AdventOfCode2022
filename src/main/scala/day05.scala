package aoc
import scala.util.chaining.*

object day05 extends App:
  case class Instruction(amount: Int, from: String, to: String)

  def parseInstruction(line: String): Instruction =
    line match
      case s"move $a from $f to $t" => Instruction(a.toInt, f, t)

  def execute(
      stack: Map[String, List[Char]],
      instruction: Instruction
  ): Map[String, List[Char]] =
    stack +
      (instruction.from -> stack(instruction.from).drop(instruction.amount)) +
      (instruction.to -> (stack(instruction.from)
        .take(instruction.amount)
        .reverse ++ stack(instruction.to)))

  def execute2(
      stack: Map[String, List[Char]],
      instruction: Instruction
  ): Map[String, List[Char]] =
    stack +
      (instruction.from -> stack(instruction.from).drop(instruction.amount)) +
      (instruction.to -> (stack(instruction.from).take(
        instruction.amount
      ) ++ stack(instruction.to)))

  val (stackData, instructionsData) = "day05.txt".live.span(_ != "")

  val stack = stackData
    .takeWhile(_ != "")
    .transpose
    .map(_.reverse)
    .filterNot(_.head == ' ')
    .map(x => x.head.toString() -> x.tail.filterNot(_ == ' ').reverse)
    .toMap

  def run(
      instructions: List[Instruction],
      stack: Map[String, List[Char]],
      e: (Map[String, List[Char]], Instruction) => Map[String, List[Char]]
  ): Unit =
    instructions
      .foldLeft(stack) { case (stack, instruction) =>
        e(stack, instruction)
      }
      .toList
      .sorted
      .foreach((a, x) => println(s"$a -> ${x.head}"))

  // Part 1.
  run(instructionsData.drop(1).map(parseInstruction), stack, execute)
  println("-" * 10)
  // Part 2.
  run(instructionsData.drop(1).map(parseInstruction), stack, execute2)
