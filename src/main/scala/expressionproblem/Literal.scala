package expressionproblem

trait Literal[F[_], A] {
  def literal(n: Int): F[A]
}

trait Addition[F[_], A] {
  def add(a1: F[A], a2: F[A]): F[A]
}

trait Multiplication[F[_], A] {
  def multiply(a1: F[A], a2: F[A]): F[A]
}