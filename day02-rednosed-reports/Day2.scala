import scala.io.Source._

object Day2 {
  def main(args: Array[String]) = { 
    val reports = fromFile(args(1)).getLines
      .map(_.split(" ").map(_.toInt)).toArray
    if (args(0) == "1") {
      println(reports.filter(isSafe).size)
    }
    if (args(0) == "2") { 
      println(reports.filter(isSafe).size + reports.filter(!isSafe(_)).filter(canBeDampened).size)
    }
  }
  
  def isSafe(report: Array[Int]): Boolean = { 
    var isSorted: Boolean = true
    var maxDiff: Int = Int.MinValue
    var minDiff: Int = Int.MaxValue

    val reportSorted = report.sorted.toArray

    val dir: Int = report(0) == reportSorted(0) match
      case true => 1
      case false => -1

    var i = 0
    while i < report.size-1 do {
      i += 1
      var diff = dir match 
        case 1 => report(i) - report(i-1)
        case -1 => report(i-1) - report(i)
      if (diff > 3 || diff < 1 || diff == 0) { 
        return false
      }
    }
    return true

  }

  def canBeDampened(safeReport: Array[Int]): Boolean = { 
    var i = 0
    while i < safeReport.size do {
      if (isSafe(safeReport
        .zipWithIndex
        .filter((e,j) => i != j)
        .map((e,j) => e)
        .toArray)) {
          return true
        }
      i += 1 
    }
    return false
  }
}
