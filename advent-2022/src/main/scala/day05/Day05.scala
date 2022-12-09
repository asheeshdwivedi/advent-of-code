package day05
import parser.TypeParser
import parser.TypeParser.*
object Day05:

  final case class Move(itemsToMove: Int, from: Int, to: Int)
  object Move:
    given TypeParser[String, Move] with
      def parse(input: String): Move = input match
        case s"move $n from $from to $to" => Move(n.toInt, from.toInt, to.toInt)

  def parseStack(input: Seq[String]) = input
    .takeWhile(_.nonEmpty)
    .transpose
    .filter(_.exists(_.isDigit))
    .map(s => (s.last.toString.toInt -> s.init.filter(_ != ' ')))
    .toMap

  def parseMove(input: Seq[String]) = input.dropWhile(_.nonEmpty).drop(1).map(_.as[Move])

  def removeGroup(stack: Map[Int, Seq[Char]], moves: Seq[Move]): Map[Int, Seq[Char]] = moves match
    case Nil                       => stack
    case Move(qty, from, to) :: xs =>
      val update = stack +
        (to   -> (stack(from).take(qty) ++ stack(to))) +
        (from -> stack(from).drop(qty))
      removeGroup(update, xs)

  // TODO improve we can do one by one we do not need reverse
  def removeOneByOne(stack: Map[Int, Seq[Char]], moves: Seq[Move]): Map[Int, Seq[Char]] = moves match
    case Nil                       => stack
    case Move(qty, from, to) :: xs =>
      val update = stack +
        (to   -> (stack(from).take(qty).reverse ++ stack(to))) +
        (from -> stack(from).drop(qty))
      removeOneByOne(update, xs)

  def part01(input: Seq[String]) = removeOneByOne(parseStack(input), parseMove(input))
    .toSeq
    .sortBy(_.head)
    .map(_.last.head)
    .mkString

  def part02(input: Seq[String]) = removeGroup(parseStack(input), parseMove(input))
    .toSeq
    .sortBy(_.head)
    .map(_.last.head)
    .mkString
