package net.reasoning.transfigure.test

import net.reasoning.transfigure._
import org.scalatest.FunSuite
import org.scalatest.Inside

import cats.Applicative
import cats.implicits._
import cats.syntax.applicative._

class StringParser extends FunSuite with Inside {

  test("StringParser can recognized a single character") {
    val strParser = new Object() with StringParsers
    val reader = StringReader("foobarbaz")
    val foo = strParser.string("foo")

    val result = foo(reader)
    inside(result) { case Right((str, StringReader(input))) =>
      assert(str == "foo")
      assert(input.toString == "barbaz")
    }
  }

  test("Unit lifts a value into the parser. It should not consume any input") {
    object TestParser extends StringParsers {
      def unitParser = unit(123)
    }

    val reader = StringReader("foobar")

    inside(TestParser.unitParser(reader)) { case Right((v, StringReader(input))) =>
      assert(v == 123)
      assert(input.toString == "foobar")
    }
  }

  test("Regex parser matches regex") {
    val parser = new Object() with RegexParsers
    val reader = StringReader("123foo")
    val regexParser = parser.regex("(\\d+)".r)

    inside(regexParser(reader)) { case Right((v, _)) =>
      assert(v == "123")
    }
  }

  test("Applicative parser style") {

    // AST node
    case class Node(s1: String)

    object TestParser extends StringParsers {
      // Using the applicative style
      // Reads "foo" and skips it and saves "bar"
      def test = pure(Node.apply _) <*> string("foo") *> string("bar")
    }

    val reader = StringReader("foobar")

    inside(TestParser.test(reader)) { case Right((Node(s1), _)) =>
      assert(s1 == "bar")
    }
  }

  test("Applicative with more than one parameter") {
    // AST node
    case class Node(left: String, right: String, extra: String)

    object TestParser extends StringParsers {
      def foo = string("foo")
      def bar = string("bar")
      def baz = string("baz")

      def test = pure(Node.apply _) <*> foo <*> bar <*> baz
    }

    val reader = StringReader("foobarbaz")

    inside(TestParser.test(reader)) { case Right((Node(r,l,e), _)) =>
      assert(r == "foo")
      assert(l == "bar")
      assert(e == "baz")
    }
  }
}
