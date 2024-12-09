import Grid._
import scala.io.Source._
import scala.math._

class Tile(var display: Char, var isAntinode: Boolean = false)

object Day8 { 
  def main(args: Array[String]) = {
    val file = args.size match 
      case 1 => "input.txt"
      case 2 => args(1)
    val lines = fromFile(file).getLines.toList
    val grid = makeGrid(lines, c=>Tile(c)) 
    val uniqueAntennae: Set[Char] = lines.map(row => row.toArray.filter(c => c != '.')).flatten.toSet
    uniqueAntennae.foreach(
      a => {
        val all: List[Cell[Tile]] = findAll(grid)(t=>t.display == a)
        computeAntinodes(grid, all)
      }
    )
    println(grid.map(row=>row.filter(t=>t.isAntinode).size).sum)
  }

  def computeAntinodes(grid: List[List[Tile]], all: List[Cell[Tile]]): Unit = { 
    all.toSeq.combinations(2).map{case Seq(a,b) => (a,b)}.foreach((a,b) => {
      val xDiff = abs(a.x - b.x)
      val yDiff = abs(a.y - b.y)
      
      val (x1, y1, x2, y2) = a.x >= b.x match
        case true => a.y >= b.y match
          case true => (a.x + xDiff, a.y + yDiff, b.x - xDiff, b.y - yDiff)
          case false => (a.x + xDiff, a.y - yDiff, b.x - xDiff, b.y + yDiff)
        case false => a.y >= b.y match
          case true => (a.x - xDiff, a.y + yDiff, b.x + xDiff, b.y - yDiff)
          case false => (a.x - xDiff, a.y - yDiff, b.x + xDiff, b.y + yDiff)

      get(grid)(x1,y1).foreach(c => { 
        grid(c.y)(c.x).isAntinode = true
      }) 
      get(grid)(x2,y2).foreach(c => { 
        grid(c.y)(c.x).isAntinode = true
      })
    })
  }
}
