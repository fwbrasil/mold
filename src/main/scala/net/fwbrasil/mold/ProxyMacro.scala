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
    val r = q"""
      new ..${types :+ proxyTrait} {
        override val impl = $instance
        ..${proxyTypes(c)(declarations)}
        ..${proxyMembers(c)(declarations, around)}
      }
    """
    println(r)
    c.untypecheck(r)
  }

  private def proxyMembers(c: Context)(m: List[c.Symbol], around: c.Expr[Around]) = {
    import c.universe._
    val wc = m.filter(!_.isConstructor).filter(!_.isSynthetic).collect { case m: MethodSymbol => m }
    wc.map {

      case symbol if (symbol.isLazy) =>
        q"override lazy val ${symbol.name} = impl.${symbol.name}"

      case symbol if (symbol.isAccessor && !symbol.setter.isMethod) =>
        q"override val ${symbol.name} = impl.${symbol.name}"

      case symbol =>
        val paramsNames = symbol.paramLists.map(_.map(_.name))

        val tuples = paramsNames.map {
          case params if (params.size == 0) => q"Tuple1(Unit)"
          case params if (params.size == 1) => q"Tuple1(..$params)"
          case params                       => q"(((..$params)))"
        } match {
          case params if (params.size == 0) => q"Tuple1(Unit)"
          case params if (params.size == 1) => q"Tuple1(..$params)"
          case params                       => q"(((..$params)))"
        }

        val etuples =
          for (i <- 1 to paramsNames.size if (paramsNames.size > 0)) yield {
            def term(i: Int) = TermName(s"_${i}")
            for (j <- 1 to paramsNames(i - 1).size) yield q"p.${term(i)}.${term(j)}"
          }

        val body =
          q"""
            val params = $tuples
            def invoke(p: params.type): ${symbol.returnType} = impl.${symbol.name}(...$etuples)
            $around(${symbol.name.decoded}, invoke)(params)  
          """

        val r = internal.defDef(symbol, Modifiers(Flag.OVERRIDE), body)
        println(showRaw(r))
        r
    }
  }

  private def proxyTypes(c: Context)(m: List[c.Symbol]) = {
    import c.universe._
    m.filter(_.isType).groupBy(_.name.toTypeName).collect {
      case (name, symbols) =>
        q"override type $name = impl.$name"
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
