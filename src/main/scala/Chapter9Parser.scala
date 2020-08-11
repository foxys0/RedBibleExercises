object Chapter9Parser {

  trait Parsers[ParseError, Parser[+ _]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]
    def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())
    def map[A, B](a: Parser[A])(f: A => B): Parser[B]
    def succeed[A](a: A): Parser[A] = string("").map(_ => a)
    def slice[A](p: Parser[A]): Parser[String]
    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

    /** Exercise 1 */
    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      product(p, p2).map(f.tupled) //.map { case (a, b) => f(a, b) }

    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
      ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def many: Parser[List[A]] = self.many(p)
      def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    }

  }

}
