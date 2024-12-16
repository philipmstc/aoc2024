import scala.io.Source._
import scala.collection.mutable.Map

object Day11 { 
  val cache: Map[String, BigInt] = Map()

  def main(args: Array[String]) = {
    val file = args.size > 1 match
      case true => args(1)
      case false => "input.txt"
    
    val part = args.size > 0 match
      case true => args(0) 
      case false => "1"

    val lines = fromFile(file).getLines.toList
    val input = lines(0).split(" ").toList
    // println(blinkNTimes(input, 5).mkString(" "))
    input.foreach(value=> cache.put(value, 1 + cache.getOrElseUpdate(value, BigInt(0))))
    val result = blinkNTimes(part match 
      case "1" => 25 
      case "2" => 75 
      case n => n.toInt)
    println(result)
  }


  def blinkNTimes(times: Int): BigInt = {
    times match 
      case 0 => totalSize()
      case _ => { 
        blink()
        blinkNTimes(times-1)
      }
  }

  def totalSize(): BigInt = { 
    cache.map((value, count) => count).sum
  }

  def blink() = { 
    val updates: List[(String, List[String], BigInt)] = cache.filter((value,count) => count > 0).map((value, count) => {
      val newStones: List[String] = value match 
        case "0" => List("1")
        case even if even.size % 2 == 0 => { 
          List(even.substring(0, (even.size / 2)), 
               BigInt(even.substring(even.size/2, even.size)).toString)
        }
        case x => List((BigInt(x)*2024).toString)
      (value, newStones, count)
    }).toList

    updates.foreach((v,n,c) => {
      cache.put(v, 0)
    })

    updates.foreach((v,n,c) => { 
      n.foreach(nn => cache.put(nn, c + cache.getOrElseUpdate(nn, 0)))
    })
  }
}
