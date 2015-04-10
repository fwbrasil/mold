package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object ProxyMacro {

  def proxy[T](c: Context)(instance: c.Expr[Any], around: c.Expr[Around])(implicit t: c.WeakTypeTag[T]) =
    createProxy(c)(proxyBaseClasses[T](c)(instance), instance, around)

  private def createProxy(c: Context)(types: List[c.Type], instance: c.Expr[Any], around: c.Expr[Around]) = {
    import c.universe._
    if (types.isEmpty)
      c.error(c.enclosingPosition, "The proxied class must have an empty constructor or implement traits.")
    val declarations = instance.actualType.members.toList.filter(_.overrides.nonEmpty)
    val typeParams = instance.actualType.typeSymbol.asClass.typeParams.zip(instance.actualType.typeArgs).toMap
    println(typeParams)
    val r = q"""
      new ..${types :+ proxyTrait(c)} {
        override val impl = $instance
        ..${proxyTypes(c)(declarations)}
        ..${proxyMembers(c)(declarations, around, typeParams)}
      }
    """
    val u = c.untypecheck(r)
    println(u)
    u
  }

  private def proxyTrait(c: Context) =
    c.mirror.symbolOf[Proxy].toType

  private def proxyMembers(c: Context)(m: List[c.Symbol], around: c.Expr[Around], typeParamsMap: Map[c.Symbol, c.Type]) = {
    import c.universe._
    val wc = m.filter(!_.isConstructor).collect { case m: MethodSymbol if (!m.isFinal && m.name.decoded != "finalize" && m.name.decoded != "clone") => m }
    wc.map {

      case symbol if (symbol.isLazy) =>
        q"override lazy val ${symbol.name} = impl.${symbol.name}"

      case symbol if (symbol.isAccessor && !symbol.setter.isMethod) =>
        q"override val ${symbol.name} = impl.${symbol.name}"

      case symbol if (symbol.isGetter) =>
        c.abort(c.enclosingPosition, "Can't proxy a type that has vars.")

      case symbol =>

        val typeParams = symbol.typeParams.map(_.name)

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

        val call =
          if (symbol.isProtectedThis || symbol.isPrivateThis)
            q"???"
          else
            q"impl.${symbol.name}[..$typeParams](...$etuples)"

        val body =
          q"""
            val params = $tuples
            def invoke(p: params.type) = $call
            $around(${symbol.name.decoded}, invoke)(params)  
          """
        val mods =
          if (symbol.overrides.nonEmpty)
            Modifiers(Flag.OVERRIDE)
          else
            Modifiers()
        removeDefaultParams(c)(internal.defDef(symbol, mods, body), typeParamsMap)
    }
  }

  private def removeDefaultParams(c: Context)(defDef: c.universe.DefDef, typeParams: Map[c.Symbol, c.Type]) = {
    import c.universe._
    val vparamss: List[List[ValDef]] = defDef.vparamss.map(_.map {
      case param if (param.mods.hasFlag(Flag.IMPLICIT)) =>
        q"implicit val ${param.name}: ${param.tpe}"
      case param =>
        q"val ${param.name}: ${param.tpe}"
    })
    val typeParamsByName = typeParams.map(t => t._1.name.decodedName -> t._2).toMap
    val tparams = defDef.tparams.map { param =>
      val rhs =
        param.rhs match {
          case TypeBoundsTree(lo, hi) =>
            TypeBoundsTree(TypeTree(typeParamsByName.getOrElse(lo.symbol.name.decodedName, lo.tpe)), TypeTree(typeParamsByName.getOrElse(hi.symbol.name.decodedName, hi.tpe)))
          case other => other
        }
      TypeDef(param.mods, param.name, param.tparams, rhs)
    }
    DefDef(defDef.mods, defDef.name, tparams, vparamss, TypeTree(), defDef.rhs)
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
      if (hasEmptyConstructor(c)(instance.actualType) && !instance.actualType.typeSymbol.isFinal && !instance.actualType.typeSymbol.asClass.isSealed)
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
