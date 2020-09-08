package expressionproblem

object Interpreter {

  object Evaluate {
    val dsl: Expression[Int] = new Expression[Int] {
      override def literal(n: Int): Int = n
      override def add(a1: Int, a2: Int): Int = a1 + a2
      override def multiply(a1: Int, a2: Int): Int = a1 * a2
    }
  }

  object Print {
    val dsl: Expression[String] = new Expression[String] {
      override def literal(n: Int): String = s"$n"
      override def add(a1: String, a2: String): String = s"($a1 + $a2)"
      override def multiply(a1: String, a2: String): String = s"($a1 * $a2)"
    }
  }
}
