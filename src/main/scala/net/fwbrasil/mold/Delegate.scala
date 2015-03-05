package net.fwbrasil.mold

import scala.reflect.macros.whitebox.Context
import language.dynamics
import language.experimental.macros
import scala.reflect.runtime.{ universe => ru }

trait Delegate extends Dynamic {

  val delegate: T forSome { type T }

  def selectDynamic(selection: String): Any = macro DelegateMacro.delegateSelectDynamic
  def applyDynamic(selection: String)(args: Any*): Any = macro DelegateMacro.delegateApplyDynamic
  def around[T, P <: Params](selection: String, params: P)(f: P => T) = f(params)

  override def toString = delegate.toString
  override def hashCode = delegate.hashCode
  override def equals(other: Any) = delegate.equals(other)
}