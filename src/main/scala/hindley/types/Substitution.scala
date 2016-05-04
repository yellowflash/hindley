package hindley.types

trait Substitution extends Function[TypeVariable, Type] {
  def compose(another:Substitution) = {
    val self = this
    new Substitution {
      override def apply(variable: TypeVariable): Type = self.substitute(another.apply(variable))
    }
  }
  def substitute(typ: Type):Type = typ match {
    case t:TypeVariable => this.apply(t)
    case Arrow(from, to) => Arrow(substitute(from), substitute(to))
  }
}

object Substitution {
  def ident = new Substitution {
    override def apply(variable: TypeVariable): Type = variable
  }
  def delta(toSub:TypeVariable, sub:Type) = new Substitution {
    override def apply(variable: TypeVariable): Type = if(toSub == variable) sub else variable
  }
}