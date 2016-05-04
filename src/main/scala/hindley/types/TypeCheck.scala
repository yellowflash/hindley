package hindley.types

import java.util.Scanner

import hindley.Parser

import scala.util.{Failure, Success}

object TypeCheck {

  def ident[A](a:A) = a
  val scanner = new Scanner(System.in)
  def main(args:Array[String]) = {
    while(scanner.hasNext) {
      val line = scanner.nextLine()
      println(Type.check(Parser.parse(line), Map.empty) match {
        case Success((subst, typ)) => Type.prettify(typ).toString
        case Failure(ex) => ex.getMessage()
      })
    }
  }
}
