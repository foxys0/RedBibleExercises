import scala.util.matching.Regex

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
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
    def label[A](msg: String)(p: Parser[A]): Parser[A]
    def scope[A](msg: String)(p: Parser[A]): Parser[A]
    def attempt[A](p: Parser[A]): Parser[A]

    /** Exercise 1 */
    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      product(p, p2).map(f.tupled) //.map { case (a, b) => f(a, b) }

    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

    /** Exercise 6 */
    def digits: Parser[String] = "\\d+".r

    /** Exercise 7 */
    def productViaFlatMap[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      for { a <- p; b <- p2 } yield (a, b) //p.flatMap(a => p2.map(b => (a, b)))

    def map2ViaFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      for { a <- p; b <- p2 } yield f(a, b) //p.flatMap(a => p2.map(b => f(a, b)))

    /** Exercise 8 */
    def mapViaFlatMap[A, B](p: Parser[A])(f: A => B): Parser[B] =
      p.flatMap(a => succeed(f(a))) //p.flatMap(f andThen succeed)

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
      ParserOps(f(a))

    implicit def regex(r: Regex): Parser[String]

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def many: Parser[List[A]] = self.many(p)
      def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def label(msg: String): Parser[A] = self.label(msg)(p)
      def scope(msg: String): Parser[A] = self.scope(msg)(p)
    }
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError = ParseError(List((this, msg)))
    def advanceBy(n: Int): Location = copy(offset = offset + n)
  }

  case class ParseError(stack: List[(Location, String)] = List()) {
    def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)
    def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
    def latest: Option[(Location, String)] = stack.lastOption
    def latestLoc: Option[Location] = latest.map(_._1)
  }

}
