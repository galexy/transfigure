package net.reasoning.transfigure

import scala.util.matching.Regex


trait Parsers {

  type Input
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
  type Input = StringReader

  def string(str: String): Parser[String] = input => {
    if (input.stream.startsWith(str)) Right((str, input.advance(str.length)))
    else Left(ParserError(s"Expected $str"))
  }
}
