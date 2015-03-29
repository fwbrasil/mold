package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object ProxyMacro {

  def proxy[T](c: Context)(instance: c.Expr[Any], around: c.Expr[Around])(implicit t: c.WeakTypeTag[T]) =
    createProxy(c)(proxyBaseClasses[T](c)(instance), instance, around)

  private def createProxy(c: Context)(types: List[c.Type], instance: c.Expr[Any], around: c.Expr[Around]) = {
    import c.universe._
    if (types.isEmpty)
      c.error(c.enclosingPosition, "The proxied class must have an empty constructor or implement interfaces.")
    val proxyTrait = c.mirror.symbolOf[Proxy].toType
    val declarations = types.map(_.decls).flatten
    q"""
      new ..${types :+ proxyTrait} {
        override val impl = $instance
        ..${proxyTypes(c)(declarations)}
        ..${proxyMembers(c)(declarations, around)}
      }
    """
  }

  private def proxyMembers(c: Context)(m: List[c.Symbol], around: c.Expr[Around]) = {
    import c.universe._
    val wc = m.filter(!_.isConstructor).collect { case m: MethodSymbol => m }
    val g = wc.groupBy(s => (s.name, s.typeParams.map(c.internal.typeDef(_)), s.paramLists.map(_.map(s => methodParam(c)(s)))))
    g.map {
      case ((name, List(), List()), symbols) if (symbols.exists(_.isLazy)) =>
        q"override lazy val $name = impl.$name"
      case ((name, List(), List()), symbols) if (symbols.exists(_.isAccessor) && symbols.exists(!_.setter.isMethod)) =>
        q"override val $name = impl.$name"
      case ((name, List(), List()), symbols) if (symbols.exists(_.isGetter)) =>
        q"def $name(implicit d: DummyImplicit) = impl.$name"
      case ((name, List(), List(param)), symbols) if (symbols.exists(_.isSetter)) =>
        q"def $name(..$param)(implicit d: DummyImplicit) = impl.$name(..$param)"
      case ((name, List(), params), symbols) if (params.flatten.isEmpty) =>
        q"override def $name = $around(${name.decoded}, (u: Unit) => impl.$name)(())"
      case ((name, typeParams, params), symbols) if (params.flatten.isEmpty) =>
        q"override def $name[..$typeParams] = $around(${name.decoded}, (u: Unit) => impl.$name[..${typeParams.map(_.name)}])(())"
      case ((name, typeParams, params), symbols) =>
        q"override def $name[..$typeParams](...$params) = $around(${name.decoded}, impl.$name[..${typeParams.map(_.name)}])(...$params)"
    }
  }

  private def proxyTypes(c: Context)(m: List[c.Symbol]) = {
    import c.universe._
    m.filter(_.isType).groupBy(_.name.toTypeName).collect {
      case (name, symbols) =>
        q"override type $name = impl.$name"
    }
  }

  private def methodParam(c: Context)(s: c.Symbol) = {
    import c.universe._
    println("AAAAAAAAAAAAAAAAAAAAAAAAAAAA", s)
    if (s.isImplicit)
      q"${s.name.toTermName}: ${paramType(c)(s)}"
    else
      q"${s.name.toTermName}: ${paramType(c)(s)}"
  }

  private def paramType(c: Context)(s: c.Symbol) = {
    import c.universe._
    s.typeSignature match {
      case TypeRef(ThisType(_), name, List()) =>
        q"impl.$name"
      case t =>
        q"$t"
    }
  }

  private def proxyBaseClasses[T](c: Context)(instance: c.Expr[Any])(implicit t: c.WeakTypeTag[T]) = {
    import c.universe._
    if (t.tpe =:= typeOf[Nothing])
      if (hasEmptyConstructor(c)(instance.actualType) && !instance.actualType.typeSymbol.isFinal)
        List(instance.actualType)
      else
        instance.actualType.baseClasses.filter(_.asClass.isTrait).map(instance.actualType.baseType(_))
    else if (instance.actualType <:< t.tpe)
      List(t.tpe)
    else
      c.abort(c.enclosingPosition, s"The value doesn't implement the proxied type.")
  }

  private def hasEmptyConstructor(c: Context)(t: c.Type) = {
    import c.universe._
    t.decls.collect {
      case m: MethodSymbol if (m.isConstructor && m.paramLists.flatten.isEmpty) => m
    }.nonEmpty
  }
}
