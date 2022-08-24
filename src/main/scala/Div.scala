case class Div(num: Expr, denom: Expr) extends Expr {
  override def neval(prec: Int): BigDecimal =
    this.num.neval(prec) / this.denom.neval(prec)
  override def toLatex: String =
    "\\frac{" + this.num.toLatex + "}{" + this.denom.toLatex + "}"
  override def toString(): String =
    s"${this.num.toString()}/${this.denom.toString()}"
}

object Div:
  def apply(num: Expr, denom: Expr): Div =
    new Div(num, denom)