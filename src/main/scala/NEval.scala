// Java/Scala's BigDecimal is missing a couple important arbitrary-precision
// functions. We implement these here. These are not the best algorithms, but
// they're simple enough to understand without a ton of background, which is the
// goal.
import java.math.MathContext
import scala.language.implicitConversions
import scala.math.BigDecimal

object Constants {
  def e(prec: Int): BigDecimal =
    // Like pi, there's probably better series out there, but we'll use a simple
    // Taylor series that converges quickly. Using the definition of e from
    // calculus, we have that e = 1 + 1/1 + 1/2 + 1/6 + 1/24 + ...
    // We continue, leapfrogging, until our value converges
    var prev = BigDecimal(1, new MathContext(prec + 2))
    var ans = BigDecimal(2, new MathContext(prec + 2))
    var denom = BigDecimal(1, new MathContext(prec + 2)); // last denominator
    var i = 1; // i! is denom
    while (prev != ans) {
      i += 1
      denom *= i
      prev = ans
      ans += 1 / denom
    }
    ans.apply(new MathContext(prec))

  def ln10(prec: Int): BigDecimal =
    // ln(1 + x) = x - x^2/2 + x^3/3 + ...
    // shift close to 1, because this is very slow otherwise
    // ln(x) = 2 ln(sqrt(x))
    // here we use the 32nd root of 10, so we introduce 3 guard digits for multiplying by 32
    val N_SQRTS = 5;
    // log(10)/log(2) is approximately 10/3, so 2^N_SQRTS is roughly 10^(10/3 * N_SQRTS)
    val GUARD = (N_SQRTS * 10) / 3 + 1
    var x = BigDecimal(10, new MathContext(prec + GUARD))
    for (i <- 0 to N_SQRTS) {
      x = x.sqrt(prec + GUARD)
    }

    var ans = x.logTaylor(prec + GUARD)
    for (i <- 0 to N_SQRTS) {
      ans = ans * 2
    }
    ans.apply(new MathContext(prec))
}

case class BigDecimalNEval(d: BigDecimal) {
  def sqrt(prec: Int): BigDecimal =
    // Compute square root using simple bisection algorithm
    if (d < 0) {
      throw new ArithmeticException(
        "Cannot take square root of negative number"
      )
    } else if (d == 0) {
      return BigDecimal(0, new MathContext(prec))
    } else {
      // bisection: crude but effective
      // use 1 extra digit of precision to ensure correct rounding
      var lo = BigDecimal(0, new MathContext(prec + 1))
      var hi = d.apply(new MathContext(prec + 1))
      var mid = lo + (hi - lo) / 2
      var mid_sqr = mid * mid
      while (mid != lo && mid != hi) {
        // println(s"${lo} ≤ ${mid} ≤ ${hi}, ${mid_sqr}")
        if (mid_sqr < d) {
          // restrict to upper half of search range
          lo = mid
        } else if (mid_sqr == d) {
          // got lucky, early return
          return mid.apply(new MathContext(prec))
        } else {
          // restrict to lower half of search range
          hi = mid
        }
        mid = lo + (hi - lo) / 2
        mid_sqr = mid * mid
      }
      mid.apply(new MathContext(prec))
    }

  def logTaylor(prec: Int): BigDecimal =
    // use a Taylor series: the base case
    // only good for x = 1 + eps, with small eps
    // log(1 + x) = x - x^2 / 2 + x^3 / 3 - ...
    // this is much slower than the exp sequence to converge, unfortunately
    val x = d.apply(new MathContext(prec + 2))
    var prev = BigDecimal(0, new MathContext(prec + 2))
    var ans = x - 1
    var num = x - 1
    var denom = BigDecimal(1, new MathContext(prec + 2)); // last denominator
    while (prev != ans) {
      num *= x - 1
      // 1, -2, 3, -4, ...
      denom = -(denom + denom.signum)

      prev = ans
      ans += num / denom
    }
    ans.apply(new MathContext(prec))

  def log(prec: Int): BigDecimal =
    // first deal with invalid inputs
    if (d <= 0) {
      throw new ArithmeticException("Cannot take log of nonpositive number")
    } else if (d < 1) {
      // ln x = -ln(1 / x)
      -((1 / d).log(prec))
    } else if (d == 1) {
      return BigDecimal(0, new MathContext(prec))
    } else if (d < 10) {
      // argument shift to get within range for logTaylor
      // ln(x) = 2 ln(sqrt(x))
      // square roots aren't cheap, but this series doesn't converge quickly: need to set threshold for more square roots not helping
      // completely arbitrarily, I've set that threshold at 1 + 0.5 / (prec + 1)
      if (d < 1 + 0.5 / (prec + 1.0)) {
        d.logTaylor(prec)
      } else {
        (2 * d.sqrt(prec + 1).log(prec + 1)).apply(new MathContext(prec))
      }
    } else {
      // ln(10 * x) = ln(10) + ln(x)
      (Constants.ln10(prec + 1) + (d / 10).log(prec + 1))
        .apply(new MathContext(prec))
    }

  // def exp: BigDecimal =
  //   // https://netlib.org/fdlibm/e_exp.c
  //   // we just steal the basic idea: for any a, b, exp(a + b) = exp(a) exp(b) and exp(a ln(b)) = b^a
  //   // first, find
}

given Conversion[BigDecimal, BigDecimalNEval] = BigDecimalNEval(_)
given Conversion[Double, BigDecimalNEval] = BigDecimalNEval(_)
given Conversion[Int, BigDecimalNEval] = BigDecimalNEval(_)
given Conversion[Long, BigDecimalNEval] = BigDecimalNEval(_)
