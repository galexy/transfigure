package net.reasoning.transfigure

case class ReaderStream[T](input: Seq[T]) {

  def startsWith(string: Seq[T]): Boolean = input.startsWith(string)

  def advance(n: Int): ReaderStream[T] = new ReaderStream(input.drop(n))

}
