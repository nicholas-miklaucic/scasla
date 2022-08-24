// for some reason, neither Java nor Scala has arbitrary-precision exponentials,
// even though they provide an exponential class. We implement our own version:
// far less optimized than "real" CAS programs, but workable for basic purposes.

case class Pow(lhs: Expr, rhs: Expr) extends Expr {
  override def neval(prec: Int): BigDecimal =
    // use standard extended version of power function
    this.rhs.neval(prec)
  override def toLatex: String =
    this.lhs.toLatex + " ^{" + this.rhs.toLatex + "}"
  override def toString(): String =
    s"${this.lhs.toString()}^${this.rhs.toString()}"
}

object Pow:
  def apply(lhs: Expr, rhs: Expr): Pow =
    new Pow(lhs, rhs)
