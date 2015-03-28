package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object DelegateMacro {

  def delegateSelectDynamic(c: Context)(selection: c.Expr[String]) =
    delegateMethodCall(c)(selection)(List())

  def delegateApplyDynamicNamed(c: Context)(selection: c.Expr[String])(args: c.Expr[(String, Any)]*) =
    delegateMethodCall(c)(selection)(List(namedParams(c)(args.toList)))

  def delegateApplyDynamic(c: Context)(selection: c.Expr[String])(args: c.Expr[Any]*) =
    delegateMethodCall(c)(selection)(List(args.toList.map(_.tree)))

  def delegateUpdateDynamic(c: Context)(selection: c.Expr[String])(value: c.Expr[Any]) = {
    import c.universe._
    q"""
        ${c.prefix.tree}.underlying.${method(c)(selection)} = $value
    """
  }

  private def delegateMethodCall(c: Context)(selection: c.Expr[String])(args: List[List[c.universe.Tree]]) = {
    import c.universe._
    q"""
        ${c.prefix.tree}.underlying.${method(c)(selection)}(...$args)
    """
  }

  private def namedParams(c: Context)(args: List[c.Expr[(String, Any)]]) = {
    import c.universe._
    for (arg <- args) yield {
      val Expr(Apply(_, List(Literal(Constant(name: String)), value))) = arg
      q"${TermName(name)} = $value"
    }
  }

  private def method(c: Context)(selection: c.Expr[String]) = {
    import c.universe._
    val Expr(Literal(Constant(methodName: String))) = selection
    TermName(methodName)
  }
}
