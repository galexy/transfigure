package net.reasoning.transfigure.test

import net.reasoning.transfigure._
import org.scalatest.FunSuite
import org.scalatest.Inside

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
    val parser = new Object() with StringParsers
    val reader = StringReader("foobar")
    val unitParser = parser.unit(123)

    inside(unitParser(reader)) { case Right((v, StringReader(input))) =>
      assert(v == 123)
      assert(input.toString == "foobar")
    }
  }
}
