package day08

import parser.TypeParser.*

object Day08:
  final case class Matrix(xcord: Int, ycord: Int, element: Int)

  def part01(input: Seq[List[Int]]): Int =
    val (rows, cols) = (input, input.transpose)

    def isVisible(rowIndex: Int, colIndex: Int, element: Int) =
      // up
      val visTop    = cols(colIndex).take(rowIndex).forall(_ < element)
      // down
      val visBottom = cols(colIndex).drop(rowIndex + 1).forall(_ < element)
      // left
      val visLeft   = rows(rowIndex).take(colIndex).forall(_ < element)
      // right
      val visRight  = rows(rowIndex).drop(colIndex + 1).forall(_ < element)

      visTop || visBottom || visLeft || visRight

    val matrix = input.zipWithIndex.flatMap {
      case (rows, rowIndex) => rows.zipWithIndex.map {
          case (element, colIndex) => Matrix(rowIndex, colIndex, element)
        }
    }
    matrix.count(m => isVisible(m.xcord, m.ycord, m.element))

  def part02(input: Seq[List[Int]]) =
    val (rows, cols) = (input, input.transpose)

    def takeWhile(list: Seq[Int], element: Int): Int =
      def takeWhileRec(head: Int, tail: Seq[Int], count: Int): Int =
        if head >= element then count
        else if tail.isEmpty then count
        else takeWhileRec(tail.head, tail.tail, count + 1)

      if list.isEmpty then 0
      else takeWhileRec(list.head, list.tail, 1)

    def scenicScore(rowIndex: Int, colIndex: Int, element: Int) =
      // up
      val visTop    = takeWhile(cols(colIndex).take(rowIndex).reverse, element)
      // down
      val visBottom = takeWhile(cols(colIndex).drop(rowIndex + 1), element)
      // left
      val visLeft   = takeWhile(rows(rowIndex).take(colIndex).reverse, element)
      // right
      val visRight  = takeWhile(rows(rowIndex).drop(colIndex + 1), element)
      visTop * visBottom * visLeft * visRight

    val matrix = input.zipWithIndex.flatMap {
      case (rows, rowIndex) => rows.zipWithIndex.map {
          case (element, colIndex) => Matrix(rowIndex, colIndex, element)
        }
    }
    matrix.map(s => scenicScore(s.xcord, s.ycord, s.element)).max

  @main def day08Part01: Unit = part01("day08/input1".as[Seq[String]].map(_.toList.map(_.asDigit)))

  @main def day08Part02: Unit = part02("day08/input1".as[Seq[String]].map(_.toList.map(_.asDigit)))
