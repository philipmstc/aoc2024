import scala.io.Source._

object Day3 { 
  def main(args: Array[String]) = {
    val lines = fromFile(args(1)).getLines.toArray
    val sub = """\d\d?\d?""".r 
    val pattern = """mul\(\d\d?\d?,\d\d?\d?\)""".r
    if (args(0) == "1") {
      println(lines.map {
        l => pattern.findAllIn(l).map {
          m => sub.findAllIn(m).map(_.toInt).product
        }.sum
      }.sum)
    } 

    if (args(0) == "2") {
      val x: Array[String] = lines.map { 
        l => "do()" + l
      }
      val x1: Array[Array[String]] = x.map {
        l => l.split("do\\(\\)")
      }
      val x2: Array[Array[Array[String]]] = x1.map { 
        ls => ls.map(l => l.split("don't\\(\\)"))
      }
      val x3: Array[Array[String]] = x2.map {
        ls => ls.map(l => l(0))
      }
      val x4: Array[String] = x3.flatten
      println(x4.map { 
          l => 
            val s = pattern.findAllIn(l).map {
              m => sub.findAllIn(m).map(_.toInt).product
            }.sum
            s
        }.sum)
    }
  }
}
