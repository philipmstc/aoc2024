import scala.io.Source._
import collection.immutable._


object Day1
{
  def main(args: Array[String]) = { 
    val all = fromFile(args(1)).getLines
      .map(_.split(" "))
      .flatMap(_.toList)
      .filter(!_.isEmpty)
      .map(_.toInt)
      .toArray

    val left = all.zipWithIndex.filter((e,i) => i % 2 == 0).map((e,i)=>e).toArray.sorted
    val right = all.zipWithIndex.filter((e,i) => i % 2 == 1).map((e,i)=>e).toArray.sorted

    if (args(0)=="1") {
      var sum = left.zipWithIndex.map((e,i) => Math.abs(e - right(i))).sum
      println(sum)
    }

    if (args(0)=="2") { 
      var sum = left.map(l => right.filter(r => r == l).size * l).sum
      println(sum)
    }
  }
}
