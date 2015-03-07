package net.fwbrasil.mold

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object DelegateMacro {

  def delegateSelectDynamic(c: Context)(selection: c.Expr[String]) =
    delegateApplyDynamic1(c)(selection)()

  def delegateApplyDynamic2(c: Context)(selection: c.Expr[String])(args1: c.Expr[Any]*)(args2: c.Expr[Any]*) = {
    import c.universe._
    q"null"
  }

  def delegateApplyDynamic1(c: Context)(selection: c.Expr[String])(args: c.Expr[Any]*) = {
    import c.universe._
    println(args)
    val proxy = c.prefix.tree
    val method = termName(c)(selection)
    val pack = c.mirror.staticPackage(DelegateMacro.this.getClass.getPackage.getName)
    val params = TermName("Params" + args.size)
    q"""
      val p = $pack.$params(..$args)
      $proxy.around($selection, p)( 
        _.apply($proxy.delegate.$method))
    """
  }

  private def termName(c: Context)(selection: c.Expr[String]) = {
    import c.universe._
    val Expr(Literal(Constant(methodName: String))) = selection
    TermName(methodName)
  }
}
