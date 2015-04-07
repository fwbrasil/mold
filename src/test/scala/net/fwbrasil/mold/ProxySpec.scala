package net.fwbrasil.mold

import scala.reflect._

class ProxySpec extends Spec {

  "creation" - {
    "type parameter not defined" - {
      "class with default constructor" in {
        class Test
        val proxy = Proxy(new Test, dummyAround)
        proxy mustBe a[Test]
        proxy mustBe a[Proxy]
      }
      "final class with default constructor" in {
        trait Trait
        final class Test extends Trait
        val proxy = Proxy(new Test, dummyAround)
        proxy must not be a[Test]
        proxy mustBe a[Proxy]
      }
      "class that extends traits" in {
        trait Trait
        class Test(a: String) extends Trait
        val proxy = Proxy(new Test("a"), dummyAround)
        proxy must not be a[Test]
        proxy mustBe a[Trait]
        proxy mustBe a[Proxy]
      }
      "(not supported) class without default constructor and that doesn't implement traits" in {
        class Test(a: String)
        """Proxy(new Test("a"), dummyAround)""" mustNot typeCheck
      }
      "(not supported) final class that doesn't implement traits" in {
        final class Test(a: String)
        """Proxy(new Test("a"), dummyAround)""" mustNot typeCheck
      }
    }
    "type parameter defined" - {
      "the value has the same type" in {
        class Test
        val proxy = Proxy[Test](new Test, dummyAround)
        proxy mustBe a[Test]
        proxy mustBe a[Proxy]
      }
      "the value implements the type" in {
        trait Trait1
        trait Trait2
        class Test(a: String) extends Trait1 with Trait2
        val proxy = Proxy[Trait1](new Test("a"), dummyAround)
        proxy must not be a[Test]
        proxy must not be a[Trait2]
        proxy mustBe a[Trait1]
        proxy mustBe a[Proxy]
      }
      "parametrized type" in {
        trait Trait[T]
        class Test extends Trait[Int]
        val proxy: Trait[Int] = Proxy[Trait[Int]](new Test, dummyAround)
        proxy must not be a[Test]
        proxy mustBe a[Proxy]
      }
      "(not supported) compound type" in {
        trait Trait1
        trait Trait2
        class Test(a: String) extends Trait1 with Trait2
        """Proxy[Trait1 with Trait2](new Test("a"), dummyAround)""" mustNot compile
      }
      "(not supported) the value doesn't implement the type" in {
        trait Trait
        class Test
        """Proxy[Trait](new Test, dummyAround)""" mustNot compile
      }
    }
  }

  "proxied members" - {
    "type" - {
      class Test {
        type T = Int
      }
      val proxy = Proxy(new Test, dummyAround)
      val i: proxy.T = 1
      """val a: proxy.T = "a"""" mustNot typeCheck
    }
    "val" - {
      class Test {
        val i = 11
      }
      val proxy = Proxy(new Test, dummyAround)
      proxy.i mustEqual 11
    }
    //    "var" - {
    //      "using the proxy type" in {
    //        class Test {
    //          var i: Int = 11
    //        }
    //        val subject = new Test
    //        val proxy = Proxy(subject, dummyAround)
    //        proxy.i mustEqual 11
    //        proxy.i = 22
    //        proxy.i mustEqual 22
    //        subject.i mustEqual 22
    //      }
    //      "(not supported) using the proxied type" in {
    //        class Test {
    //          var i: Int = 11
    //        }
    //        val subject = new Test
    //        val proxy: Test = Proxy(subject, dummyAround)
    //        proxy.i mustEqual 11
    //        proxy.i = 22
    //        proxy.i mustEqual 22
    //        subject.i must not equal 22
    //      }
    //    }
    "method" - {
      "without params" in {
        class Test {
          def a = "a"
        }
        val proxy = Proxy(new Test, dummyAround)
        proxy.a mustEqual "a"
      }
      "empty params" in {
        class Test {
          def a() = "a"
        }
        val proxy = Proxy(new Test, dummyAround)
        proxy.a mustEqual "a"
        proxy.a() mustEqual "a"
      }
      "non-empty params" in {
        class Test {
          def a(i: Int) = "a" + i
        }
        val proxy = Proxy(new Test, dummyAround)
        proxy.a(1) mustEqual "a1"
      }
      "curried" in {
        class Test {
          def a(i: Int)(s: String) = s + i
        }
        val proxy = Proxy(new Test, dummyAround)
        proxy.a(1)("a") mustEqual "a1"
      }
      "default param" in {
        class Test {
          def a(i: Int = 10) = "a" + i
        }
        val proxy = Proxy(new Test, dummyAround)
        proxy.a() mustEqual "a10"
        proxy.a(5) mustEqual "a5"
      }
      "using existential type" - {
        "defined in the class" in {
          class Test {
            type T = Int
            def a(i: T) = "a" + i
          }
          val proxy = Proxy(new Test, dummyAround)
          proxy.a(10) mustEqual "a10"
        }
        "defined in a super class" in {
          trait Trait {
            type T = Int
          }
          class Test extends Trait {
            def a(i: T) = "a" + i
          }
          val proxy = Proxy(new Test, dummyAround)
          proxy.a(10) mustEqual "a10"
        }
        "implemented in the class" in {
          trait Trait {
            type T
            def a(i: T): String
          }
          class Test extends Trait {
            type T = Int
            def a(i: T) = "a" + i
          }
          val proxy = Proxy(new Test, dummyAround)
          proxy.a(10) mustEqual "a10"
        }
      }
      "with type params" - {
        "empty value params" in {
          class Test {
            def a[T] = "a"
          }
          val proxy = Proxy(new Test, dummyAround)
          proxy.a[Int] mustEqual "a"
        }
        "class tag" in {
          class Test {
            def aaa[T: ClassTag] = classTag[T]
          }
          val proxy = Proxy(new Test, dummyAround)
          proxy.aaa[Int] mustEqual classTag[Int]
        }
        "non-empty value params" in {
          class Test {
            def a[T: ClassTag](v: T) = (classTag[T], v)
          }
          val proxy = Proxy(new Test, dummyAround)
          proxy.a[Int](10) mustEqual (classTag[Int], 10)
        }
        "curried values" in {
          class Test {
            def a[T: ClassTag, U: ClassTag](t: T)(u: U) = (classTag[T], classTag[U], t, u)
          }
          val proxy = Proxy(new Test, dummyAround)
          proxy.a[Int, String](10)("a") mustEqual (classTag[Int], classTag[String], 10, "a")
        }
      }
      "implicit values" - {

      }
      "implicit methods" - {

      }
    }
  }

  "method interception" - {
    "without params" in {

    }
    "empty params" in {

    }
    "non-empty params" in {

    }
    "curried" in {

    }
    "default param" in {

    }
  }

  private def dummyAround =
    new Around {
      def apply[T, P](selection: String, f: P => T)(params: P) = f(params)
    }
}