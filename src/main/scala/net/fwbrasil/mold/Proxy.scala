package net.fwbrasil.mold

import scala.reflect.macros.whitebox.Context
import language.dynamics
import language.experimental.macros
import scala.reflect.runtime.{ universe => ru }
import scala.reflect.ClassTag

trait Proxy
object Proxy {
  def apply[T](instance: Any): Any = macro ProxyMacro.proxy[T]
}