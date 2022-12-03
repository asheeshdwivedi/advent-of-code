package day02

import Day02.RPS.*
import Day02.Outcome.*
import parser.TypeParser.*
import parser.TypeParser

object Day02:

  enum RPS(val score: Int):
    self =>
    def beats: RPS = self match
      case Rock     => Scissors
      case Paper    => Rock
      case Scissors => Paper

    def loses: RPS = self match
      case Rock     => Paper
      case Paper    => Scissors
      case Scissors => Rock

    case Rock     extends RPS(1)
    case Paper    extends RPS(2)
    case Scissors extends RPS(3)

  object RPS:
    given TypeParser[String, RPS] with
      def parse(input: String): RPS = from(input)

    def from(x: String): RPS = x match
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors

  enum Outcome(val score: Int):
    case Win  extends Outcome(6)
    case Draw extends Outcome(3)
    case Loss extends Outcome(0)

  object Outcome:
    given TypeParser[String, Outcome] with
      def parse(input: String): Outcome = from(input)

    def from(x: String): Outcome = x match
      case "X" => Loss
      case "Y" => Draw
      case "Z" => Win

  def score(opponent: RPS, me: RPS): Int = me.score + {
    if me.beats == opponent then Win
    else if me == opponent then Draw
    else Loss
  }.score

  def score(opponent: RPS, action: Outcome): Int = action.score + {
    action match
      case Win  => opponent.loses
      case Draw => opponent
      case Loss => opponent.beats
  }.score

  def part1(input: Seq[(RPS, RPS)]): Long     = input.map { case (opponent, me) => score(opponent, me) }.sum
  def part2(input: Seq[(RPS, Outcome)]): Long = input.map { case (opponent, action) => score(opponent, action) }.sum

  @main def day02Part1: Unit = println(part1("day02/input1".as[Seq[(RPS, RPS)]]))

  @main def day02Part2: Unit = println(part2("day02/input1".as[Seq[(RPS, Outcome)]]))
