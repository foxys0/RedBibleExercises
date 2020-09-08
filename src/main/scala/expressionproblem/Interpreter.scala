package expressionproblem

object Interpreter {

  object Evaluate {

    object Literal {
      implicit val dsl: Literal[Option, Int] = a => Some(a)
    }

    object Addition {
      val dsl: Addition[Option, Int] = (o1, o2) =>
        o1.zip(o2).map {
          case (a1, a2) => a1 + a2
      }
    }

    object Multiplication {
      val dsl: Multiplication[Option, Int] = (o1, o2) =>
        o1.zip(o2).map {
          case (a1, a2) => a1 * a2
      }
    }

  }

  object Print {
    object Expression {
      val dsl: Literal[Option, String] = n => Some(s"$n")
    }

    object Addition {
      val dsl: Addition[Option, String] = (o1, o2) =>
        o1.zip(o2).map {
          case (a1, a2) => s"($a1 + $a2)"
      }
    }

    object Multiplication {
      val dsl: Multiplication[Option, String] = (o1, o2) =>
        o1.zip(o2).map {
          case (a1, a2) => s"($a1 * $a2)"
      }
    }
  }
}
