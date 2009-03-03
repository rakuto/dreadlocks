import scala.collection.mutable.HashMap
import org.scalatest.FunSuite
import org.scalalites.dreadlocks._

class ScalaInterpreterTest extends FunSuite {
  // Testing to parse syntax of scala language
  test("Parsing arithmetic expression") {
    var program = "(2 * 3) / 4" 
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parsing expression includes boolean values.") {
    var program = "true; false;"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }

  // expression consits of some conditional branch statements
  test("Parsing expression includes if statement.") {
    var program = "if(true) { \"true\".toString }"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parsing expression includes if-else statement.") {
    var program = "if(true) { \"if\".toString } else { \"else\"}"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parsing expression includes if-else-if statement.") {
    var program = "if(true) { \"if\" } else if(false) { \"else if\" }"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }
  test("Parsing expression includes if-else-if-else statement.") {
    var program = "if(true) { \"if\" } else if(false) { \"else if\" } else { \"else\"}"
    var trees = ScalaInterpreter.parse(program)
    assert(trees.successful)
  }

  /** Scala Interpreter **/
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
