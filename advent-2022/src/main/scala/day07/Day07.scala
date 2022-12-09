package day07

import scala.collection.mutable
import parser.TypeParser.*
object Day07:

  def groupDirWithSize(input: Seq[String]) =
    val path = mutable.Stack.empty[String]
    val dir  = mutable.Map.empty[String, Long]
    input.foreach {
      case s"dir $a"                       => ()
      case "$ ls"                          => ()
      case "$ cd .."                       => path.pop
      case s"$$ cd $dir" if dir != ".."    => path.push(dir + "/")
      case s"$size $name" if size != "dir" =>
        for i <- path.indices do
          val key         = path.reverse.take(i + 1).mkString
          val value: Long = dir.getOrElse(key, 0)
          dir.update(key, value + size.toLong)
    }
    dir

  def part01(input: Seq[String]) = groupDirWithSize(input).values.filter(_ <= 100000).sum

  def part02(input: Seq[String]) =
    val dir = groupDirWithSize(input)
    dir.values.filter(_ >= dir("//") - 40_000_000).min

  @main def day07Part01: Unit = part01("day07/input1".as[Seq[String]])

  @main def day07Part02: Unit = part02("day07/input1".as[Seq[String]])
