import scala.io.Source

object Scala3 {
  // Duality of purpose => Martin Odersky - The Simple Parts
  def main(args: Array[String]): Unit = {
    //val pascalStream: Stream[Seq[Long]]
    println

    val Fmt = java.text.NumberFormat.getIntegerInstance
    val Word = "\\b[-A-Za-z']+\\b".r
    val src = "shakespeare.txt"

    val lines = Source.fromFile(src).getLines

    val res = lines
      .flatMap(Word.findAllIn)
      .filter(_.length > 0)
      .map(_.toLowerCase)
      .map((_, 1))
      .toSeq
      .groupBy(_._1)
      .toSeq
      .map(s => (s._1, s._2.map(_._2).sum))
      .filter(_._2 > 100)
      .sortBy(-_._2)
      .foreach(println)

    println(res)
  }
}
