package day06

import scala.annotation.tailrec

object Day06:

  def isNUnique(input: String, n: Int) = input.take(n).toSet.size == n

  def findMarker(length: Int, input: String): Int =
    @tailrec
    def findMarker(sofar: String, remaining: String): Int =
      if remaining.isEmpty then 0
      else if isNUnique(remaining, length) then sofar.length + length
      else findMarker(sofar + remaining.head, remaining.tail)
    findMarker(input.head.toString, input.tail)

  def part1(input: Seq[String]) = findMarker(4, input.head)
  def part2(input: Seq[String]) = findMarker(14, input.head)
