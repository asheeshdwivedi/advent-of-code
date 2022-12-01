package day01

import parser.TypeParser.*
object Day01:

  def sums(lines: Seq[String]): List[Long] = lines match
    case Nil       => Nil
    case remaining =>
      val (xs, n) = ksum(remaining)
      n :: sums(xs)

  def ksum(input: Seq[String]): (Seq[String], Long) = input match
    case Nil      => (Nil, 0)
    case "" :: xs => (xs, 0)
    case n :: xs  =>
      val (rest, runningSum) = ksum(xs)
      (rest, n.toLong + runningSum)

  def part1(input: Seq[String]): Long = sums(input).max

  def part2(input: Seq[String]): Long = sums(input).sorted.reverse.take(3).sum

  @main def dayO1Part1: Unit = println(part1("day01/input1".as[Seq[String]]))

  @main def dayO1Part2: Unit = println(part2("day01/input2".as[Seq[String]]))
