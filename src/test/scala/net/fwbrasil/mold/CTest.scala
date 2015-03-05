package net.fwbrasil.mold

import org.scalatest.FreeSpec
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import language.dynamics
import language.experimental.macros

class ZText extends FreeSpec {

  "delegate" in {

    class Bippy(val delegate: List[Int]) extends Delegate {
      def hhhj(int: Int) = "a"
    }

    val b = new Bippy(List(1, 2, 3))

    println(b.size)
    println(b.slice(1, 2))
  }

  "proxy" in {
    trait Foo {
      def bazinga(param: T): T
      def boo(a: Int)(b: Int) = a + b
      def a: String
      def b = "b"
      type T
      var varr: String
      val vall: Int
    }

    trait Bar[T] {
      lazy val a = "a"
    }

    class FooImpl extends Foo with Bar[Int] {
      object o {
        type X = Int
      }
      type T = o.X
      val vall = 11
      def varr = "v"
      def varr_=(v: String) = v
      def bazinga(param: Int) = param * 2
      override def boo(a: Int)(b: Int) = a * b
    }

    val foo = new FooImpl

    val p = Proxy(foo)

    println(p.a)
    println(p.bazinga(4))
    println(p.boo(1)(2))
    println(p.isInstanceOf[Foo])
    println(p.isInstanceOf[Bar[Int]])
    println(p.isInstanceOf[FooImpl])
  }

  "stub" in {

  }
}
