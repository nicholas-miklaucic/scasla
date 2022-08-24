import scala.language.implicitConversions

@main def hello: Unit =
  val expr: Expr = new Rational(1, 2)
  println(expr)
  println(expr.toLatex)
  println(expr.neval())
