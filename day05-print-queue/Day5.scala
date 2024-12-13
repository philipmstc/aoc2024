import scala.io.Source._
import scala.util.control.Breaks._

object Day5 { 
  def main(args: Array[String]) = { 
    val file: String = args.size > 1 match
      case true => args(1)
      case false => "input.txt"

    val lines: Array[String] = fromFile(file).getLines.toArray
    val (order: Array[String], pagesAndEmpty: Array[String]) = lines.span(!_.isEmpty)
    val pages = pagesAndEmpty.filter(!_.isEmpty)

    val oMap = order.map(s => {
      val splits = s.split("\\|").toArray
      (splits(0), splits(1))
    }).groupBy(_._1)
    .mapValues(_.map(_._2)).toMap

    val total = pages
      .filter(page => isUnSorted(page, oMap))
            .map(p => args(0) == "2" match 
              case true => sort(p, oMap)
              case false => p)
      .map(getMiddleElement)
      .sum

    println(total)
  }

  def isUnSorted(page: String, oMap: Map[String, Array[String]]): Boolean = { 
    !page.split(",").zipWithIndex.map((num, i) => 
        (i==0) match 
          case true => true 
          case false => {
            val subPage = page.split(",").slice(0, i)
            oMap.contains(num) match 
              case true => oMap(num).filter(subPage.contains).isEmpty
              case false => true
          }).filter(b => b == false).isEmpty
  }

  def sort(page: String, oMap: Map[String, Array[String]]): String = {
    val p = page.split(",").toArray
    if (p.size == 1) { 
      return p(0)
    }

    var lowest: Array[String] = p.zipWithIndex
      .filter((x,i) => isLowest(p, oMap, x, i))
      .map((x,i)=> x)

    lowest.size == p.size match 
      case false => 
        (lowest :+ sort(p.filter(n => !lowest.contains(n)).mkString(","), oMap)).mkString(",")
      case true => 
        lowest.mkString(",")
  }

  def isLowest(p: Array[String], oMap: Map[String, Array[String]], x: String, i: Int): Boolean ={ 
    val isXInOMap: Boolean = oMap.contains(x)

    val isEverythingGreaterThanX: Boolean = p.filter(n => isXInOMap && oMap(x).contains(n)).isEmpty
    isEverythingGreaterThanX 
  }

  def getMiddleElement(page: String): Int = { 
    val p = page.split(",")
    p((p.size-1)/2).toInt
  }

  
}
