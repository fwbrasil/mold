package net.fwbrasil.mold

import scala.reflect.macros.whitebox.Context
import language.dynamics
import language.experimental.macros
import scala.reflect.runtime.{ universe => ru }
import scala.reflect.ClassTag

trait Proxy
trait Around {
  def apply[T, P <: Params](selection: String, params: P)(f: P => T): T
}
object Proxy {
  def apply[T](instance: Any, around: Around): Any = macro ProxyMacro.proxy[T]
}