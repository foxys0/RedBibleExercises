package expressionproblem
import scala.util.chaining._

trait Program[A] {
  def run: A
}

object Program extends App {

  def dsl[A](implicit expression: Expression[A]): Program[A] = new Program[A] {
    import expression._

    override def run: A = multiply(
      add(literal(2), literal(3)),
      literal(4)
    )
  }

  Program.dsl(Interpreter.Print.dsl).run.tap(println)
  Program.dsl(Interpreter.Evaluate.dsl).run.tap(println)

}
