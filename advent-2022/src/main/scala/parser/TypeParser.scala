package parser

import scala.io.Source

trait TypeParser[In, Out]:
  def parse(input: In): Out

object TypeParser:
  /*
  extension[In, Out] (input: In)(using parser: TypeParser[In, Out])
    def as[T](using ev: T =:= Out): Out = parser.parse(input)
   */
  extension (input: String) def as[T](using parser: TypeParser[String, T]): T = parser.parse(input)

  extension (input: Char) def as[T](using parser: TypeParser[Char, T]): T = parser.parse(input)

  given fileParser[Out](using TypeParser[String, Out]): TypeParser[String, Seq[Out]] with
    override def parse(input: String): Seq[Out] =
      Source.fromResource(input).getLines().map(_.as[Out]).toSeq

  given TypeParser[String, Seq[String]] with
    override def parse(input: String): Seq[String] =
      Source.fromResource(input).getLines().toSeq

  given TypeParser[String, Int] with
    override def parse(input: String): Int = input.toInt

