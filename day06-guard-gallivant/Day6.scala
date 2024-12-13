import scala.io.Source._
import Grid._

class Tile(
  var display: Char,
  var visited: Boolean = false // TODO make this a set of directions visited _in_ to avoid depth calculation
)

object Day6 { 
  def main(args: Array[String]) = { 
    val file = args.size > 1 match 
      case true => args(1)
      case false => "input.txt"

    val lines = fromFile(file).getLines.toList
    val grid = makeGrid(lines, c=>Tile(c))
    val cell: Cell[Tile] = find(grid)(t=>t.display=='^').getOrElse(throw new RuntimeException("Not found"))
    println(getAndVisitNext(grid, cell, 0))
    printGrid(grid, t=>t.display match 
        case '#' => '#' 
        case _ => t.visited match 
          case true => '.'
          case false => ' '
    )
    println
    val cellGrid = grid.zipWithIndex.map((row, y) => row.zipWithIndex.map((t, x) => Cell(t, x, y)))
    args(0) match 
      case "1" => println(cellGrid.map(row=>row.map(c=>c.t).filter(t=>t.visited==true).size).sum) 
      case "2" => {
        println(cellGrid.map(row => row.filter(c=>c.t.visited==true))
          .map(row => row
            .filter(c => { 
              val newGrid = makeGrid(lines, c=>Tile(c))
              newGrid(c.y)(c.x).display = 'O'
              find(newGrid)(t=>t.display=='^') match 
                case None => false 
                case Some(sell) => {
                  val looped = getAndVisitNext(newGrid, sell, 0)
//                  if (looped) { 
//                    printGrid(newGrid, t=> t.display)
//                    println 
//                    println
//                  }
                  looped
                }
             }).size
           ).sum
         )
      } 
  }

  def getAndVisitNext(grid: List[List[Tile]], cell: Cell[Tile], depth: Int): Boolean = {
    depth > 30000 match 
      case true => true 
      case false => {
        getNext(grid, cell) match 
          case Left(x) => x
          case Right(n) => {
            grid(n.y)(n.x).visited = true
            grid(n.y)(n.x).display = grid(cell.y)(cell.x).display
            getAndVisitNext(grid, n, depth+1)
        }
      }
  }

  def getNext(grid: List[List[Tile]], cell: Cell[Tile]): Either[Boolean, Cell[Tile]] = {
    val currentDir = cell.t.display
    val oppositeDir = nextDir(nextDir(cell.t.display))
    val offsets = dirOffsets(currentDir)
    val nextY = cell.y + offsets(1)
    val nextX = cell.x + offsets(0)
    (nextY < 0 || nextX < 0 || nextY >= grid.size || nextX >= grid(nextY).size) match
      case true => Left(false)
      case false => {
        grid(nextY)(nextX).display match 
          case '#'|'O' => {
            grid(cell.y)(cell.x).display match 
              case `oppositeDir` => Left(true)
              case _ => {
                grid(cell.y)(cell.x).display = nextDir(cell.t.display)
                Right(Cell(grid(cell.y)(cell.x), cell.x, cell.y))
              }
          }
          case `currentDir` => Left(true)
          case _ => Right(Cell(grid(nextY)(nextX), nextX, nextY))
      }
  }

  def dirOffsets(pointer: Char): (Int, Int) = { 
    pointer match 
      case '^' => (0, -1)
      case '<' => (-1, 0)
      case 'v' => (0, 1)
      case '>' => (1, 0)
      case x => throw new RuntimeException("Invalid direction " + x)
  }

  def nextDir(pointer: Char): Char = {
    pointer match 
      case '^' => '>'
      case '>' => 'v'
      case 'v' => '<' 
      case '<' => '^'
      case x => throw new RuntimeException("Invalid direction " + x)
  }
}
