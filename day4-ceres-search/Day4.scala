import scala.io.Source._

object Day4 { 
  def main(args: Array[String]) = { 
    val file: String = args.size > 1 match 
      case true => args(1)
      case false => "input.txt"
      
    val lines: Array[String] = fromFile(file).getLines.toArray
    val ans: Int = args(0) match 
      case "1" => {lines.zipWithIndex
      .map{ (r,i) => r.zipWithIndex
          .map { (c,j) => 
              horizontalMatch(lines, i, j) + 
              verticalMatch(lines, i, j) + 
              diagonalMatch(lines, i, j) + 
              antidiagonalMatch(lines, i, j) 
          }.sum
      }.sum}
      case "2" => {
          lines.zipWithIndex
            .map{ (r,i) => r.zipWithIndex
                .map { (c,j) => 
                    masX(lines, i,j)
                }.sum
            }.sum
      }
      println(ans)
  }

  def masX(lines: Array[String], y:Int, x:Int): Int = { 
    if (x > lines(y).size - 3 || y > lines.size - 3) {
      return 0
    }
    if (lines(y+1)(x+1) != 'A') { 
      return 0
    }
    lines(y+2)(x+2) == opposite(lines(y)(x))
    && lines(y+2)(x+2) != lines(y)(x)
    && lines(y+2)(x) == opposite(lines(y)(x+2))
    && lines(y+2)(x) != lines(y)(x+2) match
      case true => 1 
      case false => 0
  }

  def opposite(letter: Char): Char = {
    letter match 
      case 'M' => 'S'
      case 'S' => 'M'
      case x => x
  }

  def genericMatch(lines: Array[String], y: Int, x: Int, yOff: Int, xOff: Int): Int = {
    lines(y)(x) match 
      case 'X' => {
        lines(y+yOff)(x+xOff) == 'M' 
        && lines(y+2*yOff)(x+2*xOff) == 'A' 
        && lines(y+3*yOff)(x+3*xOff) == 'S' match
          case true => 1
          case false => 0
      }
      case 'S' => {
        lines(y+yOff)(x+xOff) == 'A' 
        && lines(y+2*yOff)(x+2*xOff) == 'M' 
        && lines(y+3*yOff)(x+3*xOff) == 'X' match
          case true => 1
          case false => 0
      }
      case _ => 0
  }

  def horizontalMatch(lines: Array[String], y: Int, x: Int): Int = { 
    if (x > lines(y).size - 4) { 
      return 0
    }
    genericMatch(lines, y, x, 0, 1) 
  }

  def verticalMatch(lines: Array[String], y: Int, x: Int): Int = { 
    if (y > lines.size - 4) { 
      return 0
    }
    genericMatch(lines, y, x, 1, 0)
  } 

  def diagonalMatch(lines: Array[String], y: Int, x: Int): Int = { 
    if (y > lines.size - 4 || x > lines(y).size-4) { 
      return 0
    }
    genericMatch(lines, y, x, 1, 1)
  }
  

  def antidiagonalMatch(lines: Array[String], y: Int, x: Int): Int = { 
    if (y > lines.size-4 || x < 3) { 
      return 0
    }
    genericMatch(lines, y, x, 1, -1)
  }
}
