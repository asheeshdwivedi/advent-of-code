package day04
import parser.TypeParser.*

object Day04:

  def parse(input: String): (Set[Int], Set[Int]) = input match
    case s"$a-$b,$c-$d" => ((a.toInt to b.toInt).toSet, (c.toInt to d.toInt).toSet)

  def isSubSet(setA: Set[Int], setB: Set[Int]) = (setB subsetOf setA) || (setA subsetOf setB)

  def hasOverlap(setA: Set[Int], setB: Set[Int]): Boolean = (setA intersect setB).nonEmpty

  def solve(input: Seq[String], f: (Set[Int], Set[Int]) => Boolean): Int = input.map(parse)
    .foldLeft(0)((count, t) => if f(t.head, t.last) then count + 1 else count)

  def part1(input: Seq[String]): Int = solve(input, isSubSet)

  def part2(input: Seq[String]): Int = solve(input, hasOverlap)

  @main def day04Part1: Unit = println(part1("day04/input1".as[Seq[String]]))
  @main def day04Part2: Unit = println(part2("day04/input1".as[Seq[String]]))
