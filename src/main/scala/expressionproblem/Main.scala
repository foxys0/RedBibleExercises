package expressionproblem

import cats.Id
import cats.data.EitherNec
import expressionproblem.Interpreter.{Evaluate, Print}

import scala.util.chaining._

object Main extends App {

  def line(): Unit = println("—" * 100)
  def success(s: Any): Unit = println(Console.BLUE + s.toString + Console.RESET)
  def error(s: Any): Unit = println(Console.RED + s.toString + Console.RESET)

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
    .dsl[EitherNec[String, *], Int](
      Evaluate.Literal.dsl,
      Evaluate.Addition.dsl,
      Evaluate.Multiplication.dsl,
      Evaluate.Division.dsl,
      Evaluate.Negation.dsl
    )
    .run
    .tap(success)

  line()

  ProgramDivisionByZero
    .dsl[Id, String](Print.Literal.dsl, Print.Addition.dsl, Print.Division.dsl)
    .run
    .tap(println)

  ProgramDivisionByZero
    .dsl[EitherNec[String, *], Int](
      Evaluate.Literal.dsl,
      Evaluate.Addition.dsl,
      Evaluate.Division.dsl)
    .run
    .tap(error)

  line()

}
