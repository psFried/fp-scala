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
