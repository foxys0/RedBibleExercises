package expressionproblem

trait Expression[A] {
  def literal(n: Int): A
}

trait Addition[A] {
  def add(a1: A, a2: A): A
}

trait Multiplication[A] {
  def multiply(a1: A, a2: A): A
}