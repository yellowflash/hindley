package hindley.types

import hindley.{Abstraction, Application, Expression, TermVariable}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

sealed trait Type {
  def vars:Set[TypeVariable]
}

case class TypeVariable(name:String) extends Type {
  override def vars: Set[TypeVariable] = Set(this)
  override def toString = name
}
case class Arrow(from: Type, to:Type) extends Type {
  override def vars: Set[TypeVariable] = from.vars ++ to.vars
  override def toString = s"($from -> $to)"
}

object Type {
  val nameSource :NameSource = new LargeNameSource

  def unify(first:Type, next:Type, substitution: Substitution):Try[Substitution] = {
    (substitution.substitute(first), substitution.substitute(next)) match {
      case (f: TypeVariable, s) if !s.vars.contains(f) => Success(Substitution.delta(f, s).compose(substitution))
      case (f, s: TypeVariable) => unify(s, f, substitution)
      case (Arrow(a, b), Arrow(c, d)) => for (
        first <- unify(a, c, substitution);
        next <- unify(first.substitute(b), first.substitute(d), first)
      ) yield next
      case _ => Failure(new RuntimeException(s"Couldn't unify $first and $next"))
    }
  }

  def check(exp: Expression, env: Map[TermVariable, Type]): Try[(Substitution, Type)] = {
    lazy val newName = TypeVariable(nameSource.nextName)
    exp match {
      case v: TermVariable => Success((Substitution.ident, env(v)))
      case Application(body, arg) => for (
        (bodySubst, bodyResult) <- check(body, env);
        (argSubst, argResult) <- check(arg, env);
        unified <- unify(bodyResult, Arrow(argResult, newName), argSubst.compose(bodySubst))
      ) yield (unified, unified.substitute(newName))
      case Abstraction(param, body) => for(
        (subst, result) <- check(body, env + (param -> newName))
      ) yield (subst, subst.substitute(Arrow(newName, result)))
    }
  }
  
  def prettify(typ : Type) = {
    val mapping = mutable.Map.empty[TypeVariable, TypeVariable]
    val nameSource = new PrettyNameSource
    def doPrettify(typ: Type):Type = typ match {
      case v: TypeVariable if mapping contains v => mapping(v)
      case v: TypeVariable => mapping.put(v, TypeVariable(nameSource.nextName)); mapping(v)
      case Arrow(from, to) => Arrow(doPrettify(from), doPrettify(to))
    }
    doPrettify(typ)
  }
}
