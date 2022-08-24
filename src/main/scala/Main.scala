import scala.language.implicitConversions

@main def hello: Unit =
  val expr: Expr = new LiteralNum(1) / 2 + 3
  println(expr)
  println(expr.toLatex)
  println(expr.neval())
  println(2.sqrt(6))
  println(Constants.e(10))
  println(Constants.e(100).log(100))
  println(Constants.ln10(10))
