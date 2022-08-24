case class Add(lhs: Expr, rhs: Expr) extends Expr {
  override def neval(prec: Int): BigDecimal =
    this.lhs.neval(prec) + this.rhs.neval(prec)
  override def toLatex: String =
    this.lhs.toLatex + " + " + this.rhs.toLatex
  override def toString(): String =
    s"${this.lhs.toString()} + ${this.rhs.toString()}"
}

object Add:
  def apply(lhs: Expr, rhs: Expr): Add =
    new Add(lhs, rhs)