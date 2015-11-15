package hindley


case class Binding(vars:Map[TermVariable,Expression]=Map.empty) {
  def apply(variable: TermVariable)  = vars.getOrElse(variable, variable)
  def +(more:(TermVariable, Expression)) = Binding(vars + more)
}

sealed trait Expression {
  def freeVariables:Set[TermVariable]
  def reduce(binding: Binding = Binding()):Expression
}

class Thunk(expr:Expression, binding: Binding) extends Expression {
  lazy val evaluated = expr.reduce(binding)
  override def freeVariables = expr.freeVariables
  override def reduce(binding: Binding) = this
  override def toString = evaluated.toString
}

case class TermVariable(name:String) extends Expression {
  override def freeVariables= Set(this)
  override def reduce(binding:Binding) = binding(this)
  override def toString = name
}

case class Abstraction(param: TermVariable, exp: Expression) extends Expression {
  override def freeVariables = exp.freeVariables - param
  override def reduce(binding: Binding) = Abstraction(param, exp.reduce(binding))
  override def toString = s"(\\${param}. ${exp})"
}

case class Application(fn: Expression, arg: Expression) extends Expression {
  override def freeVariables = fn.freeVariables ++ arg.freeVariables
  override def reduce(binding: Binding) = fn match {
    case Abstraction(param, expr) => expr.reduce(binding + (param -> new Thunk(arg,binding)))
    case _ => Application(fn.reduce(binding), arg.reduce(binding))
  }
  override def toString = s"(${fn} ${arg})"
}
