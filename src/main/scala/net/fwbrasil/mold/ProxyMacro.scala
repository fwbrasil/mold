package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object ProxyMacro {

  def proxy[T](c: Context)(instance: c.Expr[Any], around: c.Expr[Around])(implicit t: c.WeakTypeTag[T]) =
    createProxy(c)(proxyBaseClasses[T](c)(instance), instance, around)

  private def createProxy(c: Context)(types: List[c.Type], instance: c.Expr[Any], around: c.Expr[Around]) = {
    import c.universe._
    val proxyTrait = c.mirror.symbolOf[Proxy].toType
    val declarations = types.map(_.decls).flatten
    val res = q"""
      new ..${types :+ proxyTrait} {
        ..${proxyTypes(c)(declarations, instance)}
        ..${proxyMembers(c)(declarations, instance, around)}
      }
    """
    println(res)
    res
  }

  private def proxyMembers(c: Context)(m: List[c.Symbol], instance: c.Expr[Any], around: c.Expr[Around]) = {
    import c.universe._
    val wc = m.filter(!_.isConstructor).collect { case m: MethodSymbol => m }
    val g = wc.groupBy(s => (s.name, s.typeParams.map(c.internal.typeDef(_)), s.paramLists.map(_.map(s => methodParam(c)(s, instance)))))
    g.map {
      case ((name, List(), List()), symbols) if (symbols.exists(_.isLazy)) =>
        q"override lazy val $name = $instance.$name"
      case ((name, List(), List()), symbols) if (symbols.exists(_.isAccessor) && symbols.exists(!_.setter.isMethod)) =>
        q"override val $name = $instance.$name"
      case ((name, List(), params), symbols) if (params.flatten.isEmpty) =>
        q"override def $name = $around(${name.decoded}, (u: Unit) => $instance.$name)(())"
      case ((name, typeParams, params), symbols) if (params.flatten.isEmpty) =>
        q"override def $name[..$typeParams] = $around(${name.decoded}, (u: Unit) => $instance.$name[..${typeParams.map(_.name)}])(())"
      case ((name, typeParams, params), symbols) =>
        q"override def $name[..$typeParams](...$params) = $around(${name.decoded}, $instance.$name[..${typeParams.map(_.name)}])(...$params)"
    }
  }

  private def proxyTypes(c: Context)(m: List[c.Symbol], instance: c.Expr[Any]) = {
    import c.universe._
    m.filter(_.isType).groupBy(_.name.toTypeName).collect {
      case (name, symbols) =>
        q"type $name = $instance.$name"
    }
  }

  private def methodParam(c: Context)(s: c.Symbol, instance: c.Expr[Any]) = {
    import c.universe._
    s.typeSignature match {
      case TypeRef(ThisType(tpe), name, List()) =>
        q"${s.name.toTermName}: $instance.$name"
      case t =>
        q"${s.name.toTermName}: ${t}"
    }
  }

  private def proxyBaseClasses[T](c: Context)(instance: c.Expr[Any])(implicit t: c.WeakTypeTag[T]) = {
    import c.universe._
    if (t.tpe =:= typeOf[Nothing])
      instance.actualType.baseClasses.filter(_.asClass.isTrait).map(instance.actualType.baseType(_))
    else
      List(t.tpe)
  }
}
