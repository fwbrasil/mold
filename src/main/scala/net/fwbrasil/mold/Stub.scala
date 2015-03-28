package net.fwbrasil.mold

import scala.reflect.macros.whitebox.Context
import language.dynamics
import language.experimental.macros
import scala.reflect.runtime.{ universe => ru }
import scala.reflect.ClassTag

trait Stub {
  def apply[T: ClassTag](selection: String)(params: List[List[Any]]): T
}
object Stub {
  def apply[T](stub: Stub): Any = macro StubMacro.stub[T]
}