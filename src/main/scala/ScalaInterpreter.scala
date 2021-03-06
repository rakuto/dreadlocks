/**
 * ScalaInterpreter - scalalites.org
 *
 * Copyright (c) 2009 Rakuto Furutani (rakuto@scalalites.org)
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software in source
 * or binary form for any purpose with or without fee is hereby granted,
 * provided that the following conditions are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
package org.scalalites.dreadlocks

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.{StandardTokenParsers, StdTokenParsers}
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.input.{Positional, Reader, Position}
import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.collection.mutable.{HashMap, Map, Stack}
import java.lang.RuntimeException
import java.util.NoSuchElementException

/**
 * This class provides to way that provides arguments in template.
 */
object Context
{
  def empty = new HashMap[String, Any]
  def apply(elems: (String, Any)*): Map[String, Any] = empty ++ elems
}

sealed trait ScalaTokens extends StdTokens {
  // elements
  case class SymbolLit(chars: String) extends Token {
    override def toString = "'" + chars
  }
  case class CharLit(chars: String) extends Token {
    override def toString = "'" + chars + "'"
  }

  // tokens
  abstract class Term extends Positional
  // Primitive values
  abstract class PrimaryExpr extends Term
  {
    def klass: Class[_]
  }
  case class Num[T >: Double](x: T) extends Term
  {
    override def toString = x.toString
  }
  case class Bool(b: Boolean) extends Term
  {
    override def toString = b.toString
    //override def klass = classOf[Boolean]
  }
  case class Str(str: String) extends Term
  {
    override def toString = str.replace("\"", "\\\"").mkString("\"", "", "\"")
  }
  case class Char0(char: String) extends Term
  {
    override def toString = "'" + char + "'"
  }
  case class Symbol0(str: String) extends Term
  {
    override def toString = Symbol(str).toString
    //override def klass = classOf[Symbol]
  }
  case class Tuple0(elems: List[Term]) extends Term
  {
    //override def toString = elems.mkString("(", ", ", ")") 
  }

  // Types
  abstract class TType[T] extends Term
  {
    def klass: Class[T]
    override def toString = klass.toString
  }
  case class TAny extends TType[Any] 
  {
    override def klass = classOf[Any] 
    override def toString = "Any"
  }
  case class TUnit extends TType[Unit]
  {
    override def klass = classOf[Unit] 
    override def toString = "Unit"
  }
  case class TDouble extends TType[Double]
  {
    override def klass = classOf[Double] 
    override def toString = "Double"
  }
  case class TFloat extends TType[Float]
  {
    override def klass = classOf[Float] 
    override def toString = "Float"
  }
  case class TLong extends TType[Long]
  {
    override def klass = classOf[Long] 
    override def toString = "Long"
  }
  case class TInt extends TType[Int]
  {
    override def klass = classOf[Int]
    override def toString = "Int"
  }
  case class TShort extends TType[Short]
  {
    override def klass = classOf[Short] 
    override def toString = "Short"
  }
  case class TByte extends TType[Byte]
  {
    override def klass = classOf[Byte] 
    override def toString = "Byte"
  }
  case class TBoolean extends TType[Boolean]
  {
    override def klass = classOf[Boolean] 
    override def toString = "Boolean"
  }
  case class TNull extends TType[Null]
  {
    override def klass = classOf[Null] 
    override def toString = "null"
  }
  case class TString extends TType[String]
  {
    override def klass = classOf[String] 
    override def toString = "String"
  }
  case class TSeq extends TType[Seq[_]]
  {
    override def klass = classOf[Seq[_]] 
  }
  case class TMap extends TType[Map[_, _]]
  {
    override def klass = classOf[Map[_, _]] 
  }
  case class TRef(cls: Class[_]) extends TType[AnyRef]
  {
    override def klass = classOf[AnyRef]
  }
  case class TChar extends TType[Char]
  {
    override def klass = classOf[Char]
  }

  // Statements
  case class DeclVar(variable: Var, type0: Option[TType[_]], expr: Term) extends Term
  {
    override def toString = "var " + variable + 
      (type0 match {case Some(t) => ": " + t.toString.replace("class ", ""); case None => ""}) + " = " + expr
  }
  case class DeclVal(value: Var, type0: Option[TType[_]], expr: Term) extends Term
  {
    override def toString = "val " + value + 
      (type0 match {case Some(t) => ": " + t.toString.replace("class ", ""); case None => ""}) + " = " + expr
  }
  case class If(cond: Term, trueCase: List[Term], elseIfStmt: List[ElseIf], falseCase: Option[Else]) extends Term
  {
    override def toString = "if(" + cond + ") {" + trueCase + "}" + (elseIfStmt.map(_.toString).mkString)
  }
  case class ElseIf(cond: Term, expr: List[Term]) extends Term
  {
    override def toString = "else if(" + cond + ") {" + expr + "}"
  }
  case class Else(expr: List[Term]) extends Term
  {
    override def toString = "else {" + expr + "}"
  }

  // Variable
  case class Val[T](name: String, typ: TType[_]) extends Term
  case class Var(name: String, typ: TType[_]) extends Term
  {
    override def toString = name + ": " + typ
  }
  case class Apply(rev: Term, meth: String, args: List[Term]) extends Term
  {
    override def toString = rev.toString.mkString("(", "", ")") + "." + meth + args.mkString("(", ",", ")") 
  }
  case class Fun(args: List[Term], expr: List[Term]) extends Term
  {
    override def toString = "{" + args.mkString("(", ",", ")") + " => " + expr.mkString("; ") + "}"
  }

  // Operators
  abstract class Operator extends Term
  case class UnOp(op: String, r: Term) extends Operator
  case class BinOp(op: String, lhs: Term, rhs: Term) extends Operator
  {
    override def toString = lhs + op.mkString(" ", "", " ") + rhs
  }
  case class AssignOp(op: String, rev: Term, expr: Term) extends Operator
  {
    override def toString = rev + op.mkString(" ", "", " ") + expr
  }
  case class Ref(rcv: Term, arg: Term) extends Term
  {
    override def toString = rcv + arg.toString.mkString("(", "", ")")
  }
  case class RefTuple(rcv: Term, n: Int) extends Term
  {
    override def toString = rcv.toString.mkString("(", "", ").") + "_" + n
  }

  // miscellaneous
  case class Class0(name: String) extends Term
  {
    override def toString = name
  }
  case class Class1(name: String, ptype: Term) extends Term
  {
    override def toString = name + "[" + ptype + "]"
  }
}

sealed class ScalaLexical extends StdLexical with ScalaTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh

  // Overroad definition of Scala's tokens
  override def token: Parser[Token] = (
    ( (letter | '_') ~ rep( letter | digit | '_')       ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | digit ~ rep( digit )                              ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
    | '\'' ~ letter ~ '\''                              ^^ { case '\'' ~ char ~ '\'' => CharLit(char.toString) }
    | '\"' ~ rep( charSeq | chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | delim
    | failure("illegal character")
    ))

  def charSeq: Parser[String] = (
      '\\'  ~ '\"' ^^^ "\""
    | '\\'  ~ '\\' ^^^ "\\"
    | '\\'  ~ '/' ^^^ "/"
    | '\\'  ~ 'b' ^^^ "\b"
    | '\\'  ~ 'f' ^^^ "\f"
    | '\\'  ~ 'n' ^^^ "\n"
    | '\\'  ~ 'r' ^^^ "\r"
    | '\\'  ~ 't' ^^^ "\t"
  )
}

sealed class ScalaTokenParsers extends StdTokenParsers with ScalaTokens {
  //import lexical.Identifier
  type Tokens = ScalaTokens
  val lexical = new ScalaLexical

  // Define primitive tokens
  def symbolLit: Parser[String] = elem("symbol", x => x.isInstanceOf[SymbolLit]) ^^ (_.chars)
  def charLit: Parser[String]   = elem("char", x => x.isInstanceOf[CharLit]) ^^ (_.chars)
}

// }}}

/**
 * ScalaTokenParser
 *
 * This class provides parser for scala language, and interpreter.
 *
 * EBNF - syntax definition of scala language 
 *
 * Lexemes
 *    long_suffix      ::= "l" | "L"
 *    float_suffix     ::= "f" | "F"
 *    integer_suffix   ::= long_suffix | float_suffix
 *    sign             ::= "+" | "-"
 *    digit            ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 *    octal_digit      ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7"
 *    hex_digit        ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "a" | "b" | "c" | "d" | "e" | "f" |
 *                         "A" | "B" | "C" | "D" | "E" | "F
 *    octal_constant   ::= "0" {ocatal_digit}
 *    hex_constant     ::= ("0x" | "0X") hex_digit
 *    integer_constant ::= (octal_constant | hex_constant) integer_suffix
 *    number_constant  ::= integer_constant | float_constant
 *
 *    string     ::= "\"" {letter} "\""
 *    character  ::= "'" letter "'"
 *    boolean    ::= true | false 
 *    literal    ::= boolean | number_constant | string | symbol | character | "null"
 *    boolean    ::= true | false
 *    identifier ::= ["_"] {letter}
 *    type       ::= identifier
 *
 * Statements
 *    generic_class_name ::= identifier "[" generic_class_name "]" | identifier
 *    type_specifier     ::= identifier | generic_class_name | package_name "." identifier
 *    declare_variable   ::= "var" identifier [":" type_specifier] "=" expression
 *    declare_value      ::= "val" identifier [":" type_specifier] "=" expression
 *
 *    if_statement       ::= "if" "(" epression ")" "{" "}" {else_if_statement} [else_statement]
 *    else_if_statement  ::= "else if" "(" expression  ")" "{" expression "}"
 *    else_statement     ::= "else" "{" { expression } "}"
 *    statements         ::= if_statement
 *
 * Operators
 *    multiplicative_operator ::= "*" | "%" | "/"
 *    additive_operator       ::= "+" | "-"
 *    assignment_operator     ::= "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" |
 *                                "|=" | "^=" | "++=" | "--="
 * Compouned Names
 *    class_name ::= identifier
 *    arg_list ::= "(" {ident, ","} ")"
 *    typed_variable ::= variable ":" (identifier | identifier "[" typed_variable "]")
 *
 * Function
 *    anonymous_function ::= "(" {typed_variable ","} ")" "=>" { expression } 
 *                            | "{" "(" {typed_variable, ","} ")" "=>" { expression } "}" 
 *
 * Expressions
 *    variable        ::= (char | "_") { letter }
 *    primitive_value ::= variable | num | str | "(" expression ")"
 *    expression      ::= expression { additive_operator primary_value }
 *                        | expression { multiplicative_operator primary_value }
 *                        | variable { assignment_operator expression }
 *                        | statements
 *                        | primitive_value
 */
class ScalaTokenParser extends ScalaTokenParsers with ImplicitConversions
{
  lexical.reserved += ("_", "if", "else", "else if", "new", "case", "match", "true", "false", "var", "val", "null", 
                       "try", "catch", "with", "::", "yield")
  lexical.delimiters += ("(", ")", ",", "=", "{", "}", ".", "+", "-", "*", "/", "+=", "-=", "*=", "/=", 
                         "&", "|", "^", "&&", ":", "=>", "->", ">>", ">>>", "<<", ";", "[", "]", "_", "!")
                         
  private def pos[T <: Positional](p: => Parser[T]): Parser[T] = positioned(p)

  // Primitive expressions
  def typeIdent: Parser[TType[_]] = ident ^^ {
    case t => t match {
      case "Double" => new TDouble
      case "Float"  => new TFloat
      case "Long"   => new TLong
      case "Int"    => new TInt
      case "Short"  => new TShort
      case "Byte"   => new TByte
      case "String" => new TString
      case "Any"    => new TAny
      case "Char"   => new TChar
      case _        => new TRef(Class.forName(t))
    }
  }
  // Primitive values
  def str: Parser[Str]        = stringLit ^^ Str
  def char: Parser[Char0]     = charLit ^^ Char0
  def symbol: Parser[Symbol0] = symbolLit ^^ Symbol0
  def literal: Parser[Term]   = str | char | num | bool | symbol | failure("unexepcted quote after symbol")
  def floatingPointNumber: Parser[Double] = numericLit ~ "." ~ numericLit ^^ { case i ~ _ ~ f => (i + "." + f).toDouble}
  def num: Parser[Num[_]]     = floatingPointNumber ^^ { x => Num(x.toDouble) } | numericLit ^^ { x => Num(x.toInt) }
  def bool: Parser[Bool]      = ("true" | "false") ^^ {b => Bool(b == "true")}
  def null0: Parser[TNull]    = "null" ^^^ new TNull
  def variable: Parser[Var]   = ident ~ opt(":" ~> typeIdent) ^^ 
    { case n ~ Some(typ) => Var(n, typ); case n ~ None => Var(n, new TString)}

  // Operators
  def unaryOp: Parser[String]    = "!"
  def additiveOp: Parser[String] = "+" | "-"
  def multiOp: Parser[String]    = "*" | "%" | "/"
  def assignOp: Parser[String]   = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "++=" | "--="

  // Method
  def argList: Parser[List[Term]]  = "(" ~> repsep(expr, ",") <~ ")"
  def argument: Parser[List[Term]] = argList | anonFunc ^^ { case t  => List(t) }
  def anonFunc: Parser[Term] = (
    ("{" ~> opt("(") ~> repsep(variable, ",") <~ opt(")")) ~ ("=>" ~> program) <~ "}" ^^ Fun
    | ("(" ~> repsep(variable, ",") <~ ")") ~ ("=>" ~> program) ^^ Fun
  )

  // Statements
  // TODO: support other types
  def declVarStmt: Parser[DeclVar] = ("var" ~> variable) ~! opt(":" ~> typeIdent) ~! ("=" ~> expr) ^^ DeclVar
  def declValStmt: Parser[DeclVal] = ("val" ~> variable) ~! opt(":" ~> typeIdent) ~! ("=" ~> expr) ^^ DeclVal
  def ifStmt: Parser[If] = ("if" ~> "(" ~> primaryExpr <~ ")") ~ ("{" ~> program <~ "}") ~ rep(elseIfStmt) ~ opt(elseStmt) ^^ If
  def elseIfStmt: Parser[ElseIf] = ("else" ~> "if" ~> "(" ~> primaryExpr <~ ")") ~ ("{" ~> program <~ "}") ^^ ElseIf
  def elseStmt: Parser[Else]     = "else" ~> "{" ~> program <~ "}" ^^ Else
  def statements: Parser[Term]   = declVarStmt | ifStmt

  // Expressions
  def unaryExpr: Parser[UnOp]        = unaryOp ~ (bool | variable) ^^ UnOp
  def tupleExpr: Parser[Tuple0]      = ("(" ~> repsep(primaryExpr, ",") <~ ")") ^^ Tuple0
  def refExpr: Parser[Term]          = variable ~ ("(" ~> (primaryExpr)  <~ ")") ^^ Ref | refTupleExpr
  def refTupleExpr: Parser[RefTuple] = variable ~ (opt(".") ~> "_" ~> numericLit) ^^ { case t ~ idx => RefTuple(t, idx.toInt)}
  def methApplyExpr: Parser[Term]    = primaryExpr ~ rep((opt(".") ~> ident) ~ opt(argument)) ^^ reduceApplyList
  def primaryExpr: Parser[Term]      = literal | variable | unaryExpr | anonFunc | "(" ~> expr <~ ")"
  def assignExpr: Parser[Term]       = variable ~ assignOp ~ expr ^^ { case rcv ~ op ~ expr => AssignOp(op, rcv, expr) }
  def additiveExpr: Parser[Term]     = multiExpr ~! rep(additiveOp ~! expr) ^^ reduceBinOpList
  def multiExpr: Parser[Term]        = methApplyExpr ~! rep(multiOp ~! expr) ^^ reduceBinOpList
  def expr: Parser[Term] = (
    assignExpr
    | statements
    | refExpr 
    | additiveExpr
    | refExpr 
    | tupleExpr
    | primaryExpr
  )
  def program: Parser[List[Term]] = rep(rep(";") ~> expr <~ rep(";"))

  // helper methods to reduce list of tokens
  def reduceBinOpList: Term ~ List[String ~ Term] => Term = { case t ~ rs => (t /: rs)(reduceBinOp) }
  def reduceBinOp(lhs: Term, r: String ~ Term) = r match { case op ~ rhs => BinOp(op, lhs, rhs) }
  def reduceApplyList: Term ~ List[String ~ Option[List[Term]]] => Term = { case rcv ~ rs => (rcv /: rs)(reduceApply1) }
  def reduceApply1(rcv: Term, r: String ~ Option[List[Term]]) = r match { 
    case meth ~ Some(args) => Apply(rcv, meth, args) 
    case meth ~ None => Apply(rcv, meth, List[Term]())
  }

  // public API
  //private lazy val parser = phrase(program)
  def parse(input: String) = phrase(program)(new lexical.Scanner(input))
}

/**
 * ScalaInterpreter
 */
object ScalaInterpreter extends ScalaTokenParser
{
  type Context = Map[String, Any]

  def evaluate(input: String, context: Context): Any = {
    var globalContextChain = new Stack[Context]
    globalContextChain.push(context)
    def lookupContext(name: String): Option[Context] = {
      globalContextChain.find { ctx => ctx.contains(name) } match {
        case Some(ctx) => Some(ctx)
        case None => None
      }
    }
    def lookup(name: String): Any = lookupContext(name) match { 
      case Some(v) => v(name)
      case _ => throw new RuntimeException("undefined variable: " + name)
    }
    // FIXME:  Handling Binary Operation
    def foldBinOp(expr: BinOp): Any = { 
      expr match {
        case BinOp(op, lhs: Num[_], rhs: Num[_]) => 
          var ret: Any = lhs match {
            case Num(x: Int) => rhs match {
              case Num(y: Int) => op match {
                case "+" => x + y
                case "-" => x - y
                case "*" => x * y
                case "/" => x / y
                case "%" => x % y
                case _ => throw new RuntimeException(op + " is not a member of " + classOf[Int])
              }
              case _ => null
            } 
            case _ => null
          }
          if(ret == null) {
            var x = lhs.x.asInstanceOf[Double]
            var y = rhs.x.asInstanceOf[Double]
            ret = op match {
              case "+" => x + y
              case "-" => x - y
              case "*" => x * y
              case "/" => x / y
              case "%" => x % y
              case _ => throw new RuntimeException(op + " is not a member of " + classOf[Double])
            }
          }
          ret
        case BinOp(op, lhs: Str, rhs: Term) => evaluateImpl(lhs).toString + evaluateImpl(rhs).toString
        case BinOp(op, lhs: Num[_], rhs: Var) =>
          var ret: Any = null
          val r = evaluateImpl(rhs)
          if(r.isInstanceOf[String]) {
            ret = op match {
              case "+" => lhs.x.toString + r.toString
              case _ => throw new RuntimeException(op + " is not a member of " + rhs.typ.klass)
            }
          } else {
            throw new RuntimeException("unsported type: " + rhs) 
          }
          ret
        case BinOp(op, lhs, rhs) =>
          // FIXME:
          var l = evaluateImpl(lhs)
          var r = evaluateImpl(rhs)
          if(l.isInstanceOf[String] || r.isInstanceOf[String]) foldBinOp(BinOp(op, Str(l.toString), Str(r.toString)))
          else if(l.isInstanceOf[Int] || r.isInstanceOf[Double]) {
            foldBinOp(BinOp(op, Num(l), Num(r))) 
          } else {
            throw new RuntimeException("Unsupported binary operation: " + l + " " + op + " " + r) 
          }
      }
    }
    def evaluateImpl(expr: Term, returnType: Class[_]*): Any = {
      var retval = expr match {
        case s @ Str(str) => str.replaceAll("\\\\n", "\n").replaceAll("\\\\t", "\t")
        case n @ Num(x)   => x
        case b @ Bool(b0) => b0
        case bop @ BinOp(_, _, _) => foldBinOp(bop)
        case v @ Var(name, typ) => lookup(name)
        case ao @ AssignOp(op, recv: Var, expr) =>
          // TODO: case of undefined variable
          lookupContext(recv.name) match {
            case Some(context) => 
              op match {
                case "=" => context(recv.name) = evaluateImpl(expr, recv.typ.klass)
                case "+=" => context(recv.name) += evaluateImpl(expr).asInstanceOf[String]
                //case "-=" => 
                //case "*=" =>
                //case "/=" =>
                //case "%=" =>
                //case "&=" =>
                //case "|=" =>
                //case "^=" =>
                //case "++=" =>
                //case "--=" =>
                case _ => throw new RuntimeException("unsupported operand")
              }
            case None => null // never execute
          }
        case f @ Fun(args, expr) =>
          args.size match {
            case 1 =>
              var func = new Function1[AnyRef, Unit] {
                def apply(arg: AnyRef): Unit = {
                  var context = globalContextChain.top
                  context += (args(0).asInstanceOf[Var].name -> arg)
                  expr.foreach(evaluateImpl(_, classOf[Any]))
                }
              }
              func
            case _ => throw new RuntimeException("wrong number of parameters: " + args)
          }
        case a @ Apply(rcv, methName, args) => 
          var receiver = evaluateImpl(rcv)
          var ret: Any = methName match {
            case "foreach" =>
              var f:(Any) => Unit = evaluateImpl(args(0)).asInstanceOf[Any => Unit]
              try{
              globalContextChain.push(new HashMap[String, Any])
              receiver.asInstanceOf[Seq[_]].foreach(f)
              globalContextChain.pop
              } catch {
                case e => println(e) 
              }
              ()
            case "toString" => receiver.toString
            case _ =>
              try {
                import RichReflection._
                var arguments = args.map(evaluateImpl(_)).toArray
                var ret = receiver.asInstanceOf[AnyRef].getClass.invoke(methName, receiver, arguments, classOf[String])
                ret
             } catch {
                case e: NoSuchMethodException => 
                  var errmsg = "value " + methName + " is not a member of " + receiver.asInstanceOf[AnyRef].getClass
                  throw new RuntimeException(errmsg)
             }
          }
          ret
        case dv @ DeclVar(v, opt, expr) =>
          var context = globalContextChain.top
          opt match {
            case Some(typ) => context += (v.name -> evaluateImpl(expr, typ.klass))
            case None => context += (v.name -> evaluateImpl(expr)) // TODO: Type Inference
          }
        case ref @ Ref(v, arg) =>
          arg match {
            case Str(k) => 
              evaluateImpl(v).asInstanceOf[Map[String, _]].get(k) match {
                case Some(v) => v
                case None => throw new NoSuchElementException("key not found: " + k)
              }
            case Num(idx: Int) => evaluateImpl(v).asInstanceOf[Seq[_]](idx)
          }
        case ref @ RefTuple(v, idx) => evaluateImpl(v).asInstanceOf[Product].productElement(idx - 1)
        // Statements
        case If(cond, exprs, elseIfStmts, elseStmt) =>
          if(evaluateImpl(cond).asInstanceOf[Boolean]) {
            exprs.map(evaluateImpl(_, classOf[Any])).last 
          } else {
            elseIfStmts.find(evaluateImpl(_, classOf[Any]).asInstanceOf[Boolean]) match {
              case None => elseStmt match {
                case Some(els) => evaluateImpl(els)
                case None => None
              }
              case _ => None
            }
          }
        case Else(expr) => expr.map(evaluateImpl(_, classOf[Any])).last
        case UnOp(op, expr) => op match {
          case "!" => ! evaluateImpl(expr).asInstanceOf[Boolean] 
        }
        case _ => throw new RuntimeException("undefined token " + expr)
      }
      retval
    }
    (parse(input): @unchecked) match {
      case s @ Success(tokens, _) => tokens.map(evaluateImpl(_, classOf[String])).last
      case f @ Failure(error, _)  => throw new RuntimeException(error)
    }
  }

  def main(args: Array[String]) = {
    //import org.scalalites.benchmark.Benchmark
    //Benchmark.run("Benchmark of ScalaInterpreter") { b =>
      //b.report("arithmetic expression", 100) {
        //ScalaInterpreter.evaluate("3 * 3 * 4 * 5")
      //}
    //}
    //val input = """var _buf: String = ""; books.foreach{ (book: String) => _buf += book.toString + " " + " "}; _buf.toString"""
    //var ret: Any = evaluate(input, Context("books" -> List("one", "two")))
    //Console.println(ret)
  }
}
