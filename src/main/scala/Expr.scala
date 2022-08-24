/** A mathematical expression.
  */
trait Expr {

  /** Evaluates the expression to arbitrary precision.
    *
    * @param prec
    *   The number of decimal places to round to (default approximates 128-bit
    *   floating point.) Also working precision.
    * @return
    *   The expression, evaluated as BigDecimal
    */
  def neval(prec: Int = 34): BigDecimal

  /** Converts to LaTeX code.
    *
    * @return
    *   the expression as LaTeX code
    */
  def toLatex: String

  // Now the operator overloads

  def +(rhs: Expr): Expr = Add(this, rhs)
  def *(rhs: Expr): Expr = Mul(this, rhs)
  def /(rhs: Expr): Expr = Div(this, rhs)
  def -(rhs: Expr): Expr = Sub(this, rhs)
}