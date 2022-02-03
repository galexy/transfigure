package net.reasoning.transfigure

import scala.util.matching.Regex

trait RegexParsers extends StringParsers {
  def regex(re: Regex): Parser[String] = input => {
    re.findPrefixOf(input.stream) match {
      case Some(s) => Right((s, input.advance(s.length)))
      case _ => Left(ParserError(s"Couldn't match $re"))
    }
  }
}
