import scala.io.Source._

object DayX { 
  def main(args: Array[String]) = {
    val file = args.size > 1 match
      case true => args(1)
      case false => "input.txt"
    
    val part = args.size > 0 match
      case true => args(0) 
      case false => "1"

    val lines = fromFile(file).getLines.toList
  }
}
