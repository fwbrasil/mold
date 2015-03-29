package net.fwbrasil.mold

import scala.language.dynamics
import scala.language.experimental.macros

class Delegate[T](val underlying: T) extends Dynamic {

  def selectDynamic(selection: String): Any = macro DelegateMacro.delegateSelectDynamic

  def applyDynamic(selection: String)(args: Any*): Any = macro DelegateMacro.delegateApplyDynamic
  
  def applyDynamicNamed(selection: String)(args: (String, Any)*): Any = macro DelegateMacro.delegateApplyDynamicNamed
  
  def updateDynamic(selection: String)(value: Any): Any = macro DelegateMacro.delegateUpdateDynamic
}
