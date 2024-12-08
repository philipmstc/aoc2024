import scala.io.Source._

class Cell[T](val t: T, val x: Int, val y: Int)

object Grid {
  def makeGrid[T](lines: List[String], toCell: Char => T): List[List[T]] = {
    lines.map(row => row.toList.map(toCell))
  }

  def printGrid[T](grid: List[List[T]], toString: T => Char, delimiter: String = ""): Unit = { 
    grid.map(row => row.map(toString).mkString(delimiter)).foreach(println)
  }

  def find[T](grid: List[List[T]])(predicate: T => Boolean): Option[Cell[T]] = {
    for (i <- 0 until grid.size) {
      for (j <- 0 until grid(i).size) { 
        if (predicate(grid(i)(j))) { 
          return Some(Cell(grid(i)(j), j, i))
        }
      }
    }
    None
  }
}
