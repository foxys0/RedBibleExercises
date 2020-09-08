package expressionproblem

object Interpreter {
  def evaluate(e: Expression): Int = e match {
    case Expression.Literal(n) => n
    case Expression.Add(e1, e2) => evaluate(e1) + evaluate(e2)
    case Expression.Multiply(e1, e2) => evaluate(e1) * evaluate(e2)
  }

  def print(e: Expression): String = e match {
    case Expression.Literal(n) => s"$n"
    case Expression.Add(e1, e2) => s"(${print(e1)} + ${print(e2)})"
    case Expression.Multiply(e1, e2) => s"(${print(e1)} * ${print(e2)})"
  }
}
