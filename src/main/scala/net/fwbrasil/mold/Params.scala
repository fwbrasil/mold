package net.fwbrasil.mold

trait Params {
  def productIterator: Iterator[Any]
  def values = this.productIterator.toList
}
case class Params0()
  extends Params {
  def apply[R](f: => R) =
    f
}
case class Params1[T1](
  v1: T1) extends Params {
  def apply[R](f: T1 => R) =
    f(v1)
}
case class Params2[T1, T2](
  v1: T1, v2: T2) extends Params {
  def apply[R](f: (T1, T2) => R) =
    f(v1, v2)
}
case class Params3[T1, T2, T3](
  v1: T1, v2: T2, v3: T3) extends Params {
  def apply[R](f: (T1, T2, T3) => R) =
    f(v1, v2, v3)
}
case class Params4[T1, T2, T3, T4](
  v1: T1, v2: T2, v3: T3, v4: T4) extends Params {
  def apply[R](f: (T1, T2, T3, T4) => R) =
    f(v1, v2, v3, v4)
}
case class Params5[T1, T2, T3, T4, T5](
  v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) extends Params {
  def apply[R](f: (T1, T2, T3, T4, T5) => R) =
    f(v1, v2, v3, v4, v5)
}
case class Params6[T1, T2, T3, T4, T5, T6](
  v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) extends Params {
  def apply[R](f: (T1, T2, T3, T4, T5, T6) => R) =
    f(v1, v2, v3, v4, v5, v6)
}
