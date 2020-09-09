package expressionproblem
import cats._
import cats.syntax.all._
import expressionproblem.Expression._

object Interpreter {

  object Evaluate {

    object Literal {
      def dsl[F[_]: Applicative]: Literal[F, Int] = _.pure[F]
    }

    object Addition {
      def dsl[F[_]: Apply]: Addition[F, Int] =
        (o1, o2) => (o1, o2).mapN(_ + _)
    }

    object Negation {
      def dsl[F[_]: Functor]: Negation[F, Int] = _.map(-_)
    }

    object Multiplication {
      def dsl[F[_]: Apply]: Multiplication[F, Int] =
        (o1, o2) => (o1, o2).mapN(_ * _)
    }

    object Division {
      def dsl[F[_]: MonadError[*[_], String]]: Division[F, Int] =
        (o1, o2) =>
          (o1, o2).tupled.flatMap {
            case (_, 0) => "division by zero".raiseError[F, Int]
            case (a1, a2) if a1 % a2 == 0 => (a1 / a2).pure[F]
            case _ => "division with rest".raiseError[F, Int]
        }
    }

  }

  object Print {
    object Literal {
      def dsl[F[_]: Applicative]: Literal[F, String] = n => s"$n".pure[F]
    }

    object Addition {
      def dsl[F[_]: Apply]: Addition[F, String] =
        (o1, o2) => (o1, o2).mapN((a1, a2) => s"($a1 + $a2)")
    }

    object Negation {
      def dsl[F[_]: Functor]: Negation[F, String] = _.map(n => s"- $n")
    }

    object Multiplication {
      def dsl[F[_]: Apply]: Multiplication[F, String] =
        (o1, o2) => (o1, o2).mapN((a1, a2) => s"($a1 * $a2)")
    }

    object Division {
      def dsl[F[_]: Apply]: Division[F, String] =
        (o1, o2) => (o1, o2).mapN((a1, a2) => s"($a1 / $a2)")
    }
  }
}
