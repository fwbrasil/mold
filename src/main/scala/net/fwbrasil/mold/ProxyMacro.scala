package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

class ProxyMacro(val c: Context) {

  import c.universe._

  def proxy[T](instance: Expr[T], around: Expr[Around])(implicit t: WeakTypeTag[T]) =
    createProxy(t.tpe, instance, around)

  private def createProxy(typ: Type, instance: Expr[Any], around: Expr[Around]) = {
    val members =
      typ.members.toList.filter(m => !m.isFinal && !m.isPrivate && m.owner != typeOf[Any].typeSymbol && m.owner != typeOf[Object].typeSymbol)

    val r =
      q"""
      new $typ with ${typeOf[Proxy]} {
        override val impl = $instance
        private val around = $around
        ..${proxyTypes(members)}
        ..${proxyMembers(members, typ)}
      }
    """
        val u = c.untypecheck(r)
    println(u)
    u
  }

  private def proxyTypes(m: List[Symbol]) =
    m.filter(_.isType).filter(_.isAbstract).map(_.name.toTypeName).map { name =>
      q"override type $name = impl.$name"
    }

  private def proxyMembers(m: List[Symbol], typ: Type) = {

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
          if (symbol.privateWithin != NoSymbol || symbol.isProtectedThis)
            q"???"
          else
            q"impl.${symbol.name}[..$typeParams](...$etuples)"

        val body =
          q"""
            val params = $tuples
            def invoke(p: params.type) = $call
            around(${symbol.name.decoded}, invoke)(params)  
          """

        workaroundBugs(internal.defDef(symbol, Modifiers(Flag.OVERRIDE), body), typ)
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
            val typ = owner.baseType(defDef.symbol.owner)
            TypeBoundsTree(TypeTree(lo.tpe.asSeenFrom(typ, typ.typeSymbol)), TypeTree(hi.tpe.asSeenFrom(typ, typ.typeSymbol)))
          case other => other
        }
      TypeDef(param.mods, param.name, param.tparams, rhs)
    }
    DefDef(defDef.mods, defDef.name, tparams, vparamss, TypeTree(), defDef.rhs)
  }
}
