import java.math.MathContext
import scala.util.FromDigits.BigDecimalFromDigits
import scala.language.implicitConversions

/** Wrapper that allows numeric literals to be used as Exprs.
  */

case class LiteralNum[T: Numeric](n: T) extends Expr {
  override def toLatex: String = this.n.toString()
  override def neval(prec: Int): BigDecimal =
    BigDecimalFromDigits
      .fromDigits(this.n.toString())
      .apply(new MathContext(prec))
  override def toString(): String = this.n.toString()
}

given Conversion[Byte, LiteralNum[Byte]] = LiteralNum[Byte](_)
given Conversion[Short, LiteralNum[Short]] = LiteralNum[Short](_)
given Conversion[Int, LiteralNum[Int]] = LiteralNum[Int](_)
given Conversion[Long, LiteralNum[Long]] = LiteralNum[Long](_)
given Conversion[Float, LiteralNum[Float]] = LiteralNum[Float](_)
given Conversion[Double, LiteralNum[Double]] = LiteralNum[Double](_)
