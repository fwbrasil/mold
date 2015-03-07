package net.fwbrasil.mold

import org.scalatest.FreeSpec
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import language.dynamics
import language.experimental.macros

class ZText extends FreeSpec {

  "delegate" in {
    
    class Foo {
      def a(p: Int, q: Int) = p + q
      def b(a: Int) = 11
    }

    class Bippy(val delegate: Foo) extends Delegate {
      def hhhj(int: Int) = "a"
    }

    val b = new Bippy(new Foo)

    // b.a(1)(2) not supported
    println(b.b(3))
    println(b.hhhj(2))
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
      var varr = "1"
      val vall = 11
      def bazinga(param: Int) = param * 2
      override def boo(a: Int)(b: Int) = a * b
    }

    val foo = new FooImpl

    val ar = new Around {
      def apply[T, P <: Params](selection: String, params: P)(f: P => T): T = {
        println("before", selection)
        val r = f(params)
        println("after", selection)
        r
      }
    }

    val p = Proxy(foo, ar)

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
