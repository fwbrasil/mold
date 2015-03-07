package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object ProxyMacro {

  def proxy[T](c: Context)(instance: c.Expr[Any], around: c.Expr[Around])(implicit t: c.WeakTypeTag[T]) = {
    import c.universe._
    val classes = instance.actualType.baseClasses.filter(_.asClass.isTrait)
    val baseTypes =
      if (t.tpe =:= typeOf[Nothing])
        classes.map(instance.actualType.baseType(_))
      else
        List(t.tpe)
    createProxy(c)(baseTypes, instance, around)
  }

  private def createProxy(c: Context)(types: List[c.Type], instance: c.Expr[Any], around: c.Expr[Around]) = {
    import c.universe._
    val proxyType = c.mirror.symbolOf[Proxy].toType
    val declarations = types.map(_.decls).flatten
    val res = q"""
      new ..${types :+ proxyType} {
        ..${proxyTypes(c)(declarations, instance)}
        ..${proxyMethods(c)(declarations, instance, around)}
      }
    """
        println(res)
        res
  }

  private def proxyMethods(c: Context)(m: List[c.Symbol], instance: c.Expr[Any], around: c.Expr[Around]) = {
    import c.universe._
    val wc = m.filter(!_.isConstructor).collect { case m: MethodSymbol => m }
    val g = wc.groupBy(s => (s.name, s.paramLists))
    g.map {
      case ((name, List()), symbols) if (symbols.exists(_.isLazy)) =>
        q"override lazy val $name = $instance.$name"
      case ((name, List()), symbols) if (symbols.exists(_.isAccessor) && symbols.exists(!_.setter.isMethod)) =>
        q"override val $name = $instance.$name"
      case ((name, List()), symbols) =>
        q"override def $name = $instance.$name"
      case ((name, params), symbols) =>
        val paramsDecl = params.map(_.map(s => methodParam(c)(s, instance)))
        q"override def $name(...$paramsDecl) = $instance.$name(...$paramsDecl)"
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
        q"${s.name.toTermName}: $t"
    }
  }
}
