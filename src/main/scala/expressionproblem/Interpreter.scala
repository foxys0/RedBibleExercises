package expressionproblem

object Interpreter {

  object Evaluate {
    object Expression { implicit val dsl: Expression[Int] = n => n }
    object Addition { val dsl: Addition[Int] = (a1, a2) => a1 + a2 }
    object Multiplication { val dsl: Multiplication[Int] = (a1, a2) => a1 * a2 }
  }

  object Print {
    object Expression { val dsl: Expression[String] = n => s"$n" }
    object Addition { val dsl: Addition[String] = (a1, a2) => s"($a1 + $a2)" }
    object Multiplication { val dsl: Multiplication[String] = (a1, a2) => s"($a1 * $a2)" }
  }
}
