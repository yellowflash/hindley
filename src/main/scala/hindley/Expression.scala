package hindley


sealed trait Expression

case class TermVariable(name:String) extends Expression
case class Abstraction(param: TermVariable, exp: Expression) extends Expression
case class Application(fn: Expression, arg: Expression) extends Expression
