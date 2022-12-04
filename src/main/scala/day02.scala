package aoc

import util.chaining.*

object day02 extends App:

  enum Res:
    case Loose, Draw, Win

  enum Glyph:
    case Paper, Scissor, Rock

  case class Round(opponent: Glyph, you: Glyph)

  def glyph(s: String): Glyph =
    s match
      case "A" => Glyph.Rock
      case "B" => Glyph.Paper
      case "C" => Glyph.Scissor
      case "X" => Glyph.Rock
      case "Y" => Glyph.Paper
      case "Z" => Glyph.Scissor

  def res(s: String): Res =
    s match
      case "X" => Res.Loose
      case "Y" => Res.Draw
      case "Z" => Res.Win

  def glyphToPlay(opp: Glyph, goal: Res): Glyph =
    (opp, goal) match
      case (Glyph.Paper, Res.Win)     => Glyph.Scissor
      case (Glyph.Scissor, Res.Win)   => Glyph.Rock
      case (Glyph.Rock, Res.Win)      => Glyph.Paper
      case (_, Res.Draw)              => opp
      case (Glyph.Paper, Res.Loose)   => Glyph.Rock
      case (Glyph.Scissor, Res.Loose) => Glyph.Paper
      case (Glyph.Rock, Res.Loose)    => Glyph.Scissor

  def parse(row: String): Round =
    row match
      case s"$o $y" => Round(glyph(o), glyph(y))

  def parse2(row: String): Round =
    row match
      case s"$o $y" => Round(glyph(o), glyphToPlay(glyph(o), res(y)))

  def glyphScore(c: Glyph): Int =
    c match
      case Glyph.Rock    => 1
      case Glyph.Paper   => 2
      case Glyph.Scissor => 3

  def roundResult(round: Round): Res =
    round match
      case Round(Glyph.Rock, Glyph.Paper)    => Res.Win
      case Round(Glyph.Paper, Glyph.Scissor) => Res.Win
      case Round(Glyph.Scissor, Glyph.Rock)  => Res.Win
      case Round(a, b) if (a == b)           => Res.Draw
      case _                                 => Res.Loose

  def resScore(r: Res): Int =
    r match
      case Res.Loose => 0
      case Res.Draw  => 3
      case Res.Win   => 6

  // Part 1.
  "day02.txt".live
    .map(parse)
    .map(r => resScore(roundResult(r)) + glyphScore(r.you))
    .sum pipe println

  // Part 2.
  "day02.txt".live
    .map(parse2)
    .map(r => resScore(roundResult(r)) + glyphScore(r.you))
    .sum pipe println
