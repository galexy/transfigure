package net.reasoning.transfigure

abstract class ReaderStream[T] {
  def advance(n: Int): ReaderStream[T]
}

case class StringReader(stream: String) extends ReaderStream[Char] {
  def advance(n: Int) = StringReader(stream.drop(n))
}

