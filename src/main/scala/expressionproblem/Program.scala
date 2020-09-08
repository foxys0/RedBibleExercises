package expressionproblem
import expressionproblem.Expression._

import scala.util.chaining._

object Program extends App {

  val expression = Multiply(
    Add(Literal(2), Literal(3)),
    Literal(4)
  )

  Interpreter.evaluate(expression).tap(println)
  Interpreter.print(expression).tap(println)

}
