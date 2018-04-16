package net.reasoning.transfigure

trait Parsers {

  type Input = ReaderStream[Elem]
  type Elem

  case class ParserError(message: String)

  type ParserResult[+T] = Either[ParserError, (T, Input)]

  abstract class Parser[+T] extends (Input => ParserResult[T]) {
    def apply(in: Input): ParserResult[T]
  }

  def unit[T](a: T): Parser[T] = input => Right((a, input))
}

trait StringParsers extends Parsers {
  type Elem = Char

  def string(str: String): Parser[String] = input => {
    if (input.startsWith(str)) Right((str, input.advance(str.length)))
    else Left(ParserError(s"Expected $str"))
  }

}
