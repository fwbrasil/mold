package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object StubMacro {

  def stub[T](c: Context)(impl: c.Expr[Impl])(implicit t: c.WeakTypeTag[T]) = {
    import c.universe._
    val stubTrait = c.mirror.symbolOf[Stub].toType
    val declarations = t.tpe.decls.toList
    val res = q"""
      new $t with $stubTrait {
        ..${proxyMembers(c)(declarations, impl)}
      }
    """
    println(res)
    res
  }

  private def proxyMembers(c: Context)(m: List[c.Symbol], impl: c.Expr[Impl]) = {
    import c.universe._
    val wc = m.filter(!_.isConstructor).collect { case m: MethodSymbol => m }
    val g = wc.groupBy(s => (s.name, s.typeParams.map(c.internal.typeDef(_)), s.paramLists.map(_.map(s => methodParam(c)(s)))))
    g.map {
      case ((name, List(), List()), symbols) if (symbols.exists(_.isAccessor)) =>
        c.error(c.enclosingPosition, "Can not stub types with values (vals or vars).")
        q""
      case ((name, List(), params), symbols) if (params.flatten.isEmpty) =>
        q"override def $name = $impl[${symbols.head.returnType}](${name.decoded})(List())"
      case ((name, typeParams, params), symbols) if (params.flatten.isEmpty) =>
        q"override def $name[..$typeParams] = $impl[${symbols.head.returnType}](${name.decoded})(List())"
      case ((name, typeParams, params), symbols) =>
        val paramsLists = params.map(e => q"List(..$e)")
        q"override def $name[..$typeParams](...$params) = $impl[${symbols.head.returnType}](${name.decoded})(List(..$paramsLists))"
    }
  }

  private def methodParam(c: Context)(s: c.Symbol) = {
    import c.universe._
    val t = s.typeSignature
    q"${s.name.toTermName}: ${t}"
  }
}
