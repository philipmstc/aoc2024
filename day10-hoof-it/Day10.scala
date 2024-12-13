import scala.io.Source._
import Grid._

class Tile(var visited: Boolean, val display: Int, var visitedCount: Int = 0)

object Day10 { 
  def main(args: Array[String]) = {
    val file = args.size > 1 match
      case true => args(1)
      case false => "input.txt"
    
    val part = args.size > 0 match
      case true => args(0) 
      case false => "1"

    val lines = fromFile(file).getLines.toList
    val g = makeGrid(lines, c=>
        c match 
          case '.' => Tile(false, 99)
          case o => Tile(false, (o+"").toInt)
    )
    val action: Tile=>Unit = part match 
      case "1" => dat=>dat.visited=true 
      case "2" => dat=> {
        dat.visited=true
        dat.visitedCount = dat.visitedCount + 1
      }
    println(findAll(g)(_.display == 0).map(ct => {
      val sg = makeGrid(lines, c=> 
          c match 
            case '.' => Tile(false, 99) 
            case o => Tile(false,(o+"").toInt))
          orthoDFS(sg, ct, 
            (dis, dat) => (dis.display == dat.display - 1), 
            dat => action(dat))
        .flatten
        .filter(c=>c.display==9 && c.visited)
        .map(c=> part match 
            case "1" => 1
            case "2" => c.visitedCount)
        .sum
    }).sum)
  }
}
