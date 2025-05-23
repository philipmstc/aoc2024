import scala.io.Source._

object Day7 {
  def main(args: Array[String]) = {
    val file = args.size > 1 match
      case true => args(1)
      case false => "input.txt"

    val lines = fromFile(file).getLines.toList
    val mapped = lines.map(l => l.split(":")(0).toLong +: l.split(":")(1).split(" ").filter(!_.isEmpty).map(_.toLong))
    val filtered = mapped.map(row=>row.splitAt(1))
      .filter((head, tail) => isExpressable(head(0), tail))
      .map((head, tail) => head.appendedAll(tail))

    println(filtered.map(line=>line(0)).sum)
  }

  def isExpressable(head: Long, tail: Array[Long]): Boolean = { 
    isExpressableWith(head, tail, _+_) || 
    isExpressableWith(head, tail, _*_) || 
    isExpressableWith(head, tail, cat)
  }

  val cat = (h: Long, t: Long) => (t.toString + h.toString).toLong

  def isExpressableWith(head: Long, tail: Array[Long], op: (Long, Long) => Long): Boolean = { 
    tail.size match
      case 1 => head == tail(0)
      case x if x > 1 => {
        val newTail = tail.splitAt(1)
        newTail(1)(0) = op(newTail(1)(0), newTail(0)(0))
        isExpressable(head, newTail(1))
      }
  }
}
