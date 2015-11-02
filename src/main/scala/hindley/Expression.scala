package hindley


sealed trait Expression {
  def freeVariables:Set[TermVariable]
}

case class TermVariable(name:String) extends Expression {
  override def freeVariables: Set[TermVariable] = Set(this)
}
case class Abstraction(param: TermVariable, exp: Expression) extends Expression {
  override def freeVariables: Set[TermVariable] = exp.freeVariables - param
}
case class Application(fn: Expression, arg: Expression) extends Expression {
  override def freeVariables: Set[TermVariable] = fn.freeVariables ++ arg.freeVariables
}
