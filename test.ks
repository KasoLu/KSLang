let factorial = func (n, a) {
  cond {
    eq?(n, 1) -> { a }
    else -> { factorial(sub(n, 1), mul(a, n)) }
  }
}
factorial(5, 1)

