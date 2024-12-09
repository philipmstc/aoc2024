import scala.io.Source._

class Cell[T](val t: T, val x: Int, val y: Int)

object Grid {
  def makeGrid[T](lines: List[String], toCell: Char => T): List[List[T]] = {
    lines.map(row => row.toList.map(toCell))
  }

  def printGrid[T](grid: List[List[T]], toString: T => Char, delimiter: String = ""): Unit = { 
    grid.map(row => row.map(toString).mkString(delimiter)).foreach(println)
  }

  def get[T](grid: List[List[T]])(x: Int, y: Int): Option[Cell[T]] = { 
    y < 0 || x < 0 || y >= grid.size || x >= grid(y).size match
      case true => None
      case false => Some(Cell(grid(y)(x), x, y))
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

  def findAll[T](grid: List[List[T]])(predicate: T => Boolean): List[Cell[T]] = {
    grid.zipWithIndex
      .map((row,y) => row.zipWithIndex
        .filter((t, x) => predicate(grid(y)(x)))
        .map((t,x) => Cell(grid(y)(x), x, y))
        ).flatten.toList
  }
}
