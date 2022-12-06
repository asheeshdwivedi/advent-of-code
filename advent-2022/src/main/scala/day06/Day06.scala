package day06

object Day06:

  def solve(length: Int, input: String): Int =
    def doSolve(soFar: String, remaining: String): Int =
      if remaining.isEmpty() then 0
      else if remaining.take(length).toSet.size == length then soFar.length + length
      else doSolve(soFar + remaining.head, remaining.tail)
    doSolve(input.head.toString, input.tail)

  def part1(input: Seq[String]) = solve(4, input.head)
  def part2(input: Seq[String]) = solve(14, input.head)
