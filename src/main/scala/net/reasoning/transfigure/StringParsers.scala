package net.reasoning.transfigure

trait StringParsers extends Parsers {
  type Elem = Char
  type Input = StringReader

  def string(str: String): Parser[String] = input => {
    if (input.stream.startsWith(str)) Right((str, input.advance(str.length)))
    else Left(ParserError(s"Expected $str"))
  }
}
