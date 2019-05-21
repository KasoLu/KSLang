let Y-combinator =  
  func (F) {
    func (x) { x(x) } (
      func (f) { F(func (v) { f(f)(v) }) }
    )
  }

Y-combinator( 
  func (factorial) {
    func (n) {
      cond {
        eq?(n, 1) -> { 1 }
        else -> { mul(n, factorial(sub(n, 1))) }
      }
    }
  }
)(5)

