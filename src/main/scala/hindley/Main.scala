package hindley

object Main {
  def main(args:Array[String]): Unit = {
    println (Parser.parse("\\func.\\arg. func func arg"))
  }
}