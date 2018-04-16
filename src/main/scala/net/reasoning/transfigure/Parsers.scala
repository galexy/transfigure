package net.reasoning.transfigure

import scala.util.matching.Regex
import cats.Applicative
import cats.syntax.applicative._

trait Parsers {

  type Input
  type Elem

  case class ParserError(message: String)

  type ParserResult[+T] = Either[ParserError, (T, Input)]

  abstract class Parser[+T] extends (Input => ParserResult[T]) {
    def apply(in: Input): ParserResult[T]

    def <|>[U >: T](p2: Parser[U]): Parser[U] = input => this(input) match {
      case Right((res,in)) => Right((res,in))
      case Left(_) => p2(input)
    }
  }

  implicit val parserApplicative: Applicative[Parser] =
    new Applicative[Parser] {
      def pure[A](a: A):Parser[A] = input => Right((a, input))

      override def map[A,B](pa: Parser[A])(f: A => B): Parser[B] = input => {
        pa(input).map(res => (f(res._1), res._2))
      }

      def ap[A,B](pf: Parser[A => B])(pa: Parser[A]): Parser[B] = input => {
        pf(input) match {
          case Right((f, in)) => map(pa)(f)(in)
          case Left(e)    => Left(e)
        }
      }
    }

  def unit[T](a: T)(implicit app: Applicative[Parser]): Parser[T] = app.pure(a)

  def pure[T](a: T)(implicit app: Applicative[Parser]): Parser[T] = app.pure(a)
  def pure[A,B,C](a: Function2[A,B,C])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D](a: Function3[A,B,C,D])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E](a: Function4[A,B,C,D,E])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F](a: Function5[A,B,C,D,E,F])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G](a: Function6[A,B,C,D,E,F,G])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H](a: Function7[A,B,C,D,E,F,G,H])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I](a: Function8[A,B,C,D,E,F,G,H,I])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J](a: Function9[A,B,C,D,E,F,G,H,I,J])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K](a: Function10[A,B,C,D,E,F,G,H,I,J,K])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L](a: Function11[A,B,C,D,E,F,G,H,I,J,K,L])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M](a: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N](a: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](a: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](a: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](a: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](a: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](a: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](a: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](a: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](a: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V])(implicit app: Applicative[Parser]) = app.pure(a.curried)
  def pure[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W](a: Function22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W])(implicit app: Applicative[Parser]) = app.pure(a.curried)
}

trait StringParsers extends Parsers {
  type Elem = Char
  type Input = StringReader

  def string(str: String): Parser[String] = input => {
    if (input.stream.startsWith(str)) Right((str, input.advance(str.length)))
    else Left(ParserError(s"Expected $str"))
  }
}

trait RegexParsers extends StringParsers {
  def regex(re: Regex): Parser[String] = input => {
    re.findPrefixOf(input.stream) match {
      case Some(s) => Right((s, input.advance(s.length)))
      case _ => Left(ParserError(s"Couldn't match $re"))
    }
  }
}
