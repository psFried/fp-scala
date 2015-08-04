object Curry {

  def curry[A, B, C](fun: (A, B) => C): A => (B => C) = {
    (a: A) => {
      (b: B) => fun(a, b)
    }
  }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
}
