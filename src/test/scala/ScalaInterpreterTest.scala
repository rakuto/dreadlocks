import scala.collection.mutable.HashMap
import org.scalatest.FunSuite
import org.scalalites.dreadlocks._

class ScalaInterpreterTest extends FunSuite {
  // Testing to parse syntax of scala language
  test("Parse arithmetic expression") {
    var program = "(2 * 3) / 4" 
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parse expression includes boolean values.") {
    var program = "true; false;"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }

  // Test of parsing expression
  test("Parse if statement.") {
    var program = "if(true) { \"true\".toString }"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parse if-else statement.") {
    var program = "if(true) { \"if\".toString } else { \"else\"}"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parse if-else-if statement.") {
    var program = "if(true) { \"if\" } else if(false) { \"else if\" }"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parse if-else-if-else statement.") {
    var program = "if(true) { \"if\" } else if(false) { \"else if\" } else { \"else\"}"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parse expression define tuple with two elements.") {
    var program = """var t = (23, "rakuto")"""
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parse expression define variable with chracter.") {
    var program = "var ch: Char = 'c'" 
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parse expression contains reference of element in tuples.") {
    var program = "var t = (24, 25); t._1"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }

  // Scala Interpreter
  test("Evaluate arithmetic expression.") {
    var program = "1 + 2 + 3"
    var result = ScalaInterpreter.evaluate(program, Context()).asInstanceOf[Int]
    assert(result === 6)
  }
  test("Evaluate expression includes binary operation of some variables") {
    var program = "dance + dance"
    var result = ScalaInterpreter.evaluate(program, Context("dance" -> "Dance "))
    assert(result === "Dance Dance ")
  }
  test("Evaluate expression includes to reference of collection like Map") {
    var program = """ "B: " + (measurement("bust")).toString"""
    var result = ScalaInterpreter.evaluate(program, Context("measurement" -> HashMap("bust" -> 90)))
    assert(result === "B: 90")
  }
  test("Evaluate expression includes to call of method.") {
    var program = "book.toString" 
    var context = Context("book" -> "The Great Gatsby")
    var result = ScalaInterpreter.evaluate(program, context)
    assert(result === context("book"))
  }
  test("Evaluate expression includes reference of like sequence.") {
    var program = """books(0)""" 
    var context = Context("books" -> List("The Great Gatsby", "Tender is the Night"))
    var result = ScalaInterpreter.evaluate(program, context)
    assert(result === "The Great Gatsby")
  }
  test("Evaluate expression includes 'if' statement.") {
    var program = "if(true) { \"true\".toString }"
    var result = ScalaInterpreter.evaluate(program, Context())
    assert(result === "true")
  }
  test("Evaluate expression contains method-chain expression.") {
    var program = "12345.toString.substring(0, 3)"
    var result = ScalaInterpreter.evaluate(program, Context())
    assert(result === "123")
  }
}
