import org.scalatest._

class CurryTest extends FunSpec {

  describe("currying functions") {
    def myFun(a: Int, b: String): String = {
      b + "X" + a
    }

    val curried: Int => (String => String) = Curry.curry(myFun)

    it("curries a function") {
      val result = curried(4)("myString")
      assert("myStringX4" == result)
    }

    it("can also uncurry a function") {
      val uncurried = Curry.uncurry(curried)
      val result = uncurried.apply(9, "blind mice")
      assert("blind miceX9" == result)
    }

  }
}



class ComposeTest extends FunSpec {

  def compose[A, B, C](f1: A => B, f2: B => C): A => C = {
    (a: A) => f2(f1(a))
  }

  describe("composing functions") {

    def fun1(i: Int): String = {
      i.toString()
    }

    def fun2(s: String): Seq[String] = {
      Seq.fill(3)(s)
    }

    it("composes two functions") {
      val composed: Int => Seq[String] = compose(fun1, fun2)

      val result: Seq[String] = composed(5)

      assert(Seq("5", "5", "5") == result)
    }
  }
}
