package day03

import parser.TypeParser.*

object Day03:

  def halve(input: String): Seq[String] = input.take(input.length / 2) :: input.drop(input.length / 2) :: Nil

  def priority(input: Char): Int = if input.isUpper then input - 38 else input - 96

  def common(input: Seq[String]): Char = input.reduce(_ intersect _).head

  def part1(input: Seq[String]): Int = input
    .map(s => priority(common(halve(s))))
    .sum

  def part2(input: Seq[String]): Int = input
    .grouped(3)
    .map(s => priority(common(s)))
    .sum

  @main def day03Part1: Unit = println(part1("day03/input1".as[Seq[String]]))
  @main def day03Part2: Unit = println(part2("day03/input1".as[Seq[String]]))
