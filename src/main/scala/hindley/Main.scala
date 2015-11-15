package hindley

import scala.io.Source

object Main {
  def main(args:Array[String]): Unit = {
    Source.fromInputStream(System.in)
          .getLines()
          .foreach(line => {
              println(Parser.parse(line).reduce)
           })
  }
}