package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

class ProxyMacro(val c: Context) {

  import c.universe._

  def proxy[T](instance: Expr[Any], around: Expr[Around])(implicit t: WeakTypeTag[T]) =
    createProxy(proxyBaseClasses[T](instance.actualType), instance, around)

  private def createProxy(types: List[Type], instance: Expr[Any], around: Expr[Around]) = {
    
    val members =
      types.map { typ =>
        typ.typeSymbol.asClass.selfType
        typ.members.toList.map {
          case symbol: MethodSymbol =>
            val defDef = internal.defDef(symbol, Modifiers(Flag.OVERRIDE), q"???")
            workaroundBugs(defDef, typ).symbol
          case other => other
        }
      }.flatten
        .groupBy(m => (m.name, showRaw(m.typeSignature)))
        .filterNot(_._2.exists(_.isFinal))
        .filterNot(_._2.exists(_.owner == typeOf[Any].typeSymbol))
        .filterNot(_._2.exists(_.owner == typeOf[Object].typeSymbol))
        .values.map(_.head).toList

    val r =
      q"""
      new ..${types :+ c.typeOf[Proxy]} {
        override val impl = $instance
        private val around = $around
        ..${proxyTypes(members)}
        ..${proxyMembers(members)}
      }
    """
    val u = c.untypecheck(r)
    //    println(u)
    u
  }

  private def proxyMembers(m: List[Symbol]) = {

    val wc = m.filter(!_.isConstructor).collect { case m: MethodSymbol if (!m.name.decoded.contains("$default$")) => m }

    wc.map {

      case symbol if (symbol.isSetter) =>
        c.abort(c.enclosingPosition, "Can't proxy a type that has vars.")

      case symbol if (symbol.isGetter) =>
        val valDef = internal.valDef(symbol, q"impl.${symbol.name}")
        ValDef(Modifiers(Flag.OVERRIDE), valDef.name, TypeTree(), valDef.rhs)

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
          if (symbol.isProtected || symbol.isPrivate)
            q"???"
          else
            q"impl.${symbol.name}[..$typeParams](...$etuples)"

        val body =
          q"""
            val params = $tuples
            def invoke(p: params.type) = $call
            around(${symbol.name.decoded}, invoke)(params)  
          """

        internal.defDef(symbol, Modifiers(Flag.OVERRIDE), body)
    }
  }

  private def workaroundBugs(defDef: DefDef, owner: Type) = {
    val vparamss: List[List[ValDef]] = defDef.vparamss.map(_.map {
      case param if (param.mods.hasFlag(Flag.IMPLICIT)) =>
        q"implicit val ${param.name}: ${param.tpe}"
      case param =>
        q"val ${param.name}: ${param.tpe}"
    })
    val tparams = defDef.tparams.map { param =>
      val rhs =
        param.rhs match {
          case TypeBoundsTree(lo, hi) =>
            val typeParamsByName = owner.typeSymbol.asClass.typeParams.zip(owner.typeArgs).toMap
            println(typeParamsByName)
            TypeBoundsTree(TypeTree(typeParamsByName.getOrElse(lo.symbol, lo.tpe)), TypeTree(typeParamsByName.getOrElse(hi.symbol, hi.tpe)))
          case other => other
        }
      TypeDef(param.mods, param.name, param.tparams, rhs)
    }
    DefDef(defDef.mods, defDef.name, tparams, vparamss, TypeTree(), defDef.rhs)
  }

  private def proxyTypes(m: List[Symbol]) =
    m.filter(_.isType).groupBy(_.name.toTypeName).collect {
      case (name, symbols) =>
        q"override type $name = impl.$name"
    }

  private def proxyBaseClasses[T](instanceType: Type)(implicit t: WeakTypeTag[T]) = {
    if (t.tpe =:= typeOf[Nothing])
      if (isExtensible(instanceType))
        List(instanceType)
      else
        inferBaseTraits(instanceType)
    else if (instanceType <:< t.tpe)
      List(t.tpe)
    else
      c.abort(c.enclosingPosition, s"The value doesn't implement the proxied type.")
  }

  private def inferBaseTraits(typ: Type) =
    typ.baseClasses.filter(_.asClass.isTrait).map(typ.baseType(_)) match {
      case Nil =>
        c.abort(c.enclosingPosition, "The proxied class must have an empty constructor or implement traits.")
      case traits =>
        traits
    }

  private def isExtensible(typ: Type) =
    hasEmptyConstructor(typ) && !typ.typeSymbol.isFinal && !typ.typeSymbol.asClass.isSealed

  private def hasEmptyConstructor(t: Type) =
    t.decls.collect {
      case m: MethodSymbol if (m.isConstructor && m.paramLists.flatten.isEmpty) => m
    }.nonEmpty
}
