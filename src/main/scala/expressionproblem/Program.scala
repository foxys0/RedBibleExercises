package expressionproblem
import expressionproblem.Interpreter.{Evaluate, Print}

import scala.util.chaining._

trait Program[A] {
  def run: A
}

object Program extends App {

  def dsl[A](
    implicit expression: Expression[A],
    addition: Addition[A],
    multiplication: Multiplication[A]
  ): Program[A] = new Program[A] {
    import addition._
    import expression._
    import multiplication._

    override def run: A = multiply(
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
    .tap(println)

  Program
    .dsl(
      Evaluate.Expression.dsl,
      Evaluate.Addition.dsl,
      Evaluate.Multiplication.dsl
    )
    .run
    .tap(println)

}
