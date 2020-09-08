package expressionproblem

sealed trait Expression extends Product with Serializable
object Expression {
  final case class Literal(n: Int) extends Expression
  final case class Add(e1: Expression, e2: Expression) extends Expression
  final case class Multiply(e1: Expression, e2: Expression) extends Expression
}