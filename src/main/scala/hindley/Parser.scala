package hindley

import scala.util.parsing.combinator.RegexParsers


object Parser extends RegexParsers {
  def expression:Parser[Expression] =  (factor*)^^{case applications => applications.reduceLeft(Application.apply)}
  def factor = paranthesised | termVariable | abstraction
  def paranthesised = ("("~expression~")") ^^{case _~expr~_ => expr}
  def termVariable =  literal ^^ {case variable => TermVariable(variable)}
  def abstraction = ("\\"~literal~"."~expression)^^{case "\\"~argument~"."~expr => Abstraction(TermVariable(argument), expr)}

  def literal = "[a-zA-Z][^\\\\ .()]*".r

  def parse(expr:String) = parseAll(expression, expr) match {
    case Success(result, _) => result
    case Failure(msg, _) => throw new RuntimeException(msg)
    case Error(msg, _) => throw new RuntimeException(msg)
  }
}
