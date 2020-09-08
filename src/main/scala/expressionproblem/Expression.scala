package expressionproblem

trait Expression[A] {
  def literal(n: Int): A
  def add(a1: A, a2: A): A
  def multiply(a1: A, a2: A): A
}