package expressionproblem
import expressionproblem.Expression._

trait Program[F[_], A] {
  def run: F[A]
}

object Program extends App {

  def dsl[F[_], A](
    implicit L: Literal[F, A],
    A: Addition[F, A],
    M: Multiplication[F, A],
    D: Division[F, A],
    N: Negation[F, A]
  ): Program[F, A] = new Program[F, A] {
    import A._
    import D._
    import L._
    import M._
    import N._

    override def run: F[A] = negate(
      divide(
        multiply(
          add(literal(2), literal(3)),
          literal(4)
        ),
        literal(2)
      )
    )
  }

}

object ProgramDivisionByZero extends App {

  def dsl[F[_], A](implicit L: Literal[F, A], D: Division[F, A]): Program[F, A] =
    new Program[F, A] {
      import D._
      import L._

      override def run: F[A] = divide(literal(10), literal(0))
    }

}
