import day01.Day01
import day02.Day02
import day02.Day02.*
import parser.TypeParser.*

Day01.part1("day01/input1".as[Seq[String]])
Day01.part2("day01/input2".as[Seq[String]])
Day02.part1("day02/input1".as[Seq[(RPS, RPS)]])
Day02.part2("day02/input1".as[Seq[(RPS, Outcome)]])


val result2 = "day02/input1".as[Seq[String]].collect {
      case "A X" => 1 + 3
      case "A Y" => 2 + 6
      case "A Z" => 3 + 0
      case "B X" => 1 + 0
      case "B Y" => 2 + 3
      case "B Z" => 3 + 6
      case "C X" => 1 + 6
      case "C Y" => 2 + 0
      case "C Z" => 3 + 3
    }.sum

 "day02/input1".as[Seq[String]].collect {
      case "A X" => 3 + 0
      case "A Y" => 1 + 3
      case "A Z" => 2 + 6
      case "B X" => 1 + 0
      case "B Y" => 2 + 3
      case "B Z" => 3 + 6
      case "C X" => 2 + 0
      case "C Y" => 3 + 3
      case "C Z" => 1 + 6
    }.sum
