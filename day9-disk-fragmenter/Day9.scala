import scala.io.Source._
import scala.util.boundary, boundary.break

class Block(var value: Option[Int])

object Day9 { 
  def main(args: Array[String]) = {
    val file = args.size > 1 match
      case true => args(1)
      case false => "input.txt"
    
    val part = args.size > 0 match
      case true => args(0) 
      case false => "1"

    val lines = fromFile(file).getLines.toList
    val line = lines(0)
  
    val memory = line.map(c=>c+"")
      .zipWithIndex
      .flatMap((c, i) => i % 2 match
        case 0 => List.fill(c.toInt)(Block(Some(i/2)))
        case 1 => List.fill(c.toInt)(Block(None))).toList

    var tail: Int = memory.size-1

    var res = part match 
      case "1" => getRes(memory, 0, tail, List())
      case "2" => getRes2(memory, getSizes(memory))

    println(res.zipWithIndex.map((b,i) => b.value match 
      case Some(n) => BigInt(n * i)
      case None => BigInt(0)).sum)
  }

  def getRes(memory: List[Block], start: Int, end: Int, res: List[Block]): List[Block] = {
    start > end match 
      case true => res 
      case false => {
        memory(start).value match 
          case Some(n) => getRes(memory, start+1, end, res :+ memory(start))
          case None => {
            memory(end).value match
              case Some(n) => getRes(memory, start+1, end-1, res :+ memory(end))
              case None => getRes(memory, start, end - 1, res)
          }
      }
  }

  def getRes2(memory: List[Block], gapSizes: List[(Option[Int], Int)]): List[Block] = { 
    move(gapSizes, 0, gapSizes.size-1)
  }

  def move(gapSizes: List[(Option[Int], Int)], start: Int, end: Int): List[Block] = {
    end == 0 match
      case true => expand(gapSizes)
      case false => { 
        gapSizes(end)(0) match 
          case Some(n) => {
            gapSizes.zipWithIndex.find((tuple, i) => i < end && tuple(0).isEmpty && tuple(1) >= gapSizes(end)(1)) match 
              case Some(tuple, n) => {
                val v = tuple(0)
                val s = tuple(1)
                s == gapSizes(end)(1) match 
                  case true => {
                    var newGaps = gapSizes.slice(0, n)
                    newGaps = newGaps :+ gapSizes(end) 
                    newGaps = newGaps.appendedAll(gapSizes.slice(n+1, end))
                    newGaps = newGaps :+ (None, s)
                    if (end < gapSizes.size - 2) {
                      newGaps = newGaps.appendedAll(gapSizes.slice(end+1, gapSizes.size))
                    }
                    val newEnd = newGaps.size - (gapSizes.size - end) - 1
                    move(newGaps, start, newEnd)
                  }
                  case false => {
                    val newSize = s - gapSizes(end)(1)
                    var newGaps = gapSizes.slice(0, n)
                    newGaps = newGaps :+ gapSizes(end) :+ (None, newSize)
                    newGaps = newGaps.appendedAll(gapSizes.slice(n+1, end))
                    newGaps = newGaps :+ (None, gapSizes(end)(1))
                    if (end < gapSizes.size - 2) {
                      newGaps = newGaps.appendedAll(gapSizes.slice(end+1, gapSizes.size))
                    }
                    val newEnd = newGaps.size - (gapSizes.size - end) - 1
                    move(newGaps, start, newEnd)
                  }
              }
              case None => move(gapSizes, start, end -1)
          }
          case None => move(gapSizes, start, end - 1)
      }
  }

  def expand(gapSizes: List[(Option[Int], Int)]): List[Block] = { 
    gapSizes.flatMap((v, size) => List.fill(size)(Block(v)))
  }

  def getSizes(memory: List[Block]): List[(Option[Int], Int)] = { 
    var gapSizes: List[(Option[Int], Int)] = List()
    var (currentBlock, currentInd) = memory.zipWithIndex(0)
    var size = 1
    memory.zipWithIndex.filter((_, i) => i > 0).foreach((b, i) => { 
      b.value match 
        case Some(n) => currentBlock.value match
          case Some(c) => n == c match 
            case true => {
              size = size + 1
              if (i == memory.size -1) {
                gapSizes = gapSizes :+ (currentBlock.value, size)
              }
            }
            case false => { 
              gapSizes = gapSizes :+ (currentBlock.value, size)
              currentBlock = b
              size = 1
            }
          case None => {
            gapSizes = gapSizes :+ (currentBlock.value, size)
            currentBlock = b
            size = 1
          }
        case None => currentBlock.value match 
          case Some(c) => {
            gapSizes = gapSizes :+ (currentBlock.value, size)
            currentBlock = b 
            size = 1 
          }
          case None => {
            size = size + 1
            if (i == memory.size - 1) { 
              gapSizes = gapSizes :+ (currentBlock.value, size) 
            }
          }
    })
    println(gapSizes.map((b, i) => b match
      case Some(n) => n + " * " + i
      case None => ". *" + i
      ).mkString("\n"))
    gapSizes
  }
}
