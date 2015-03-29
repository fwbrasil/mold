package net.fwbrasil.mold

import scala.reflect.macros.whitebox.Context
import language.dynamics
import language.experimental.macros
import scala.reflect.runtime.{ universe => ru }
import scala.reflect.ClassTag

trait Proxy {

  val impl: Any

  override def toString = impl.toString
  override def hashCode = impl.hashCode
  override def equals(other: Any) = impl.equals(other)
}

trait Around {
  def apply[T, P](selection: String, f: P => T)(params: P): T
}

object Proxy {
  def apply[T](instance: Any, around: Around): Any = macro ProxyMacro.proxy[T]
}