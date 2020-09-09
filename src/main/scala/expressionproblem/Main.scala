package expressionproblem

import cats.Id
import expressionproblem.Interpreter.{Evaluate, Print}

import scala.util.chaining._

object Main extends App {

  def line(): Unit = println("—" * 100)

  line()

  Program
    .dsl[Id, String](
      Print.Literal.dsl,
      Print.Addition.dsl,
      Print.Multiplication.dsl,
      Print.Division.dsl,
      Print.Negation.dsl
    )
    .run
    .tap(println)

  Program
    .dsl[Either[String, *], Int](
      Evaluate.Literal.dsl,
      Evaluate.Addition.dsl,
      Evaluate.Multiplication.dsl,
      Evaluate.Division.dsl,
      Evaluate.Negation.dsl
    )
    .run
    .tap(println)

  line()

  ProgramDivisionByZero
    .dsl[Id, String](Print.Literal.dsl, Print.Division.dsl)
    .run
    .tap(println)

  ProgramDivisionByZero
    .dsl[Either[String, *], Int](Evaluate.Literal.dsl, Evaluate.Division.dsl)
    .run
    .tap(println)

}
