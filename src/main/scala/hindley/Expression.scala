package hindley


sealed trait Expression {
  def freeVariables:Set[TermVariable]
  def substitute(variable: TermVariable, expr: Expression):Expression
  def reduce:Expression
}

case class TermVariable(name:String) extends Expression {
  override def freeVariables= Set(this)
  override def substitute(variable: TermVariable, expr:Expression) = if(variable == this) expr else this
  override def reduce = this
  override def toString = name
}

object TermVariable {
  var index = 0
  def newVar = {
    index = index + 1
    TermVariable("_" + index)
  }
}

case class Abstraction(param: TermVariable, exp: Expression) extends Expression {
  override def freeVariables = exp.freeVariables - param
  override def substitute(variable: TermVariable, expr: Expression) = {
    if(expr.freeVariables contains param) this.alphaRename.substitute(variable, expr)
    else Abstraction(param, exp.substitute(variable, expr))
  }
  def alphaRename:Expression = {
    val newVariable = TermVariable.newVar
    Abstraction(newVariable, exp.substitute(param, newVariable))
  }

  override def reduce = Abstraction(param, exp.reduce)
  override def toString = s"(\\${param}. ${exp})"
}

case class Application(fn: Expression, arg: Expression) extends Expression {
  override def freeVariables = fn.freeVariables ++ arg.freeVariables
  override def substitute(variable: TermVariable, expr: Expression) = Application(fn.substitute(variable, expr), arg.substitute(variable,expr))
  override def reduce = fn match {
    case Abstraction(variable, exp) => exp.substitute(variable, arg).reduce
    case _ => Application(fn.reduce, arg.reduce)
  }
  override def toString = s"(${fn} ${arg})"
}
