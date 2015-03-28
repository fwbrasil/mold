package net.fwbrasil.mold

import org.scalatest.FreeSpec

class DelegateSpec extends Spec {

  val subject = new {
    def a = 1
    def b(v: Int) = v * 2
    def c(s: String, i: Int) = s + i
    def d(s: String)(i: Int) = s + i
    var e = 1
    val f = collection.mutable.Map(1 -> 2)
  }

  val delegate = new Delegate(subject)

  "without parameters" in {
    delegate.a mustEqual 1
  }

  "one parameter" in {
    delegate.b(2) mustEqual 4
  }

  "two parameters" in {
    delegate.c("s", 1) mustEqual "s1"
  }

  "named parameters" in {
    delegate.c(s = "s", i = 1) mustEqual "s1"
  }

  "update" in {
    delegate.e = 2
    delegate.e mustEqual subject.e
    subject.e mustEqual 2
  }

  "nested select" in {
    delegate.f(1) mustEqual 2
  }

  "nested select and update" in {
    delegate.f(1) = 3
    delegate.f mustEqual subject.f
    subject.f mustEqual Map(1 -> 3)
  }

  "(not supported) multiple parameters lists" in {
    """delegate.d("s")(1) mustEqual "s1"""" mustNot typeCheck
  }
}
