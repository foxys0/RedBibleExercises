package expressionproblem
import expressionproblem.Interpreter.{Evaluate, Print}

trait Program[F[_], A] {
  def run: F[A]
}

object Program extends App {

  def dsl[F[_], A](
    implicit L: Literal[F, A],
    A: Addition[F, A],
    M: Multiplication[F, A]
  ): Program[F, A] = new Program[F, A] {
    import A._
    import L._
    import M._

    override def run: F[A] = multiply(
      add(literal(2), literal(3)),
      literal(4)
    )
  }

  Program
    .dsl(
      Print.Expression.dsl,
      Print.Addition.dsl,
      Print.Multiplication.dsl
    )
    .run
    .foreach(println)

  Program
    .dsl(
      Evaluate.Literal.dsl,
      Evaluate.Addition.dsl,
      Evaluate.Multiplication.dsl
    )
    .run
    .foreach(println)

}
