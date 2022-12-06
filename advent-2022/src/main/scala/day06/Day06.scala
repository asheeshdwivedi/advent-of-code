package day06

object Day06:

  def findMarker(length: Int, input: String): Int =
    def findMarker(soFar: String, remaining: String): Int =
      if remaining.isEmpty() then 0
      else if remaining.take(length).toSet.size == length then soFar.length + length
      else findMarker(soFar + remaining.head, remaining.tail)
    findMarker(input.head.toString, input.tail)

  def part1(input: Seq[String]) = findMarker(4, input.head)
  def part2(input: Seq[String]) = findMarker(14, input.head)
