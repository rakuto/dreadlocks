/**
 * DreadLocks template engien - scalalites.org
 *
 *  Copyright (c) 2009 Rakuto Furutani (rakuto@scalalites.org)
 *  All rights reserved.
 *
 *  Permission to use, copy, modify, and distribute this software in source
 *  or binary form for any purpose with or without fee is hereby granted,
 *  provided that the following conditions are met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
package org.scalalites.dreadlocks

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.collection.mutable.{HashMap, Map}
import scala.util.matching.Regex
import java.io._
import java.util.regex.Pattern

object Template
{
  lazy val EXPR_PATTERN  = """[\$#]\{(.*?)\}""".r
  lazy val STMT_PATTERN  = """<\?(?:sc)(?:\s|\r?\n)(.*?)\s*\?>""".r
  //val COMPILED_CLASS_DIR = "."

  def apply(file: File)     = new Template(file)
  def apply(source: Source) = new Template(source)
  def apply(input: String)  = new Template(input)
}

/**
 * This class provides methods used by templates at runtime.
 */
class Template(source: String)
{
  type Context = Map[String, Any]

  var scripts    = new ListBuffer[String]()
  var preamble   = "_buf"
  var shorthand  = true
  var fromFile   = true
  var precompile = false
  
  //lazy val compiler: Global = new Global(new Settings)
  private var output: String = ""
  private var context: Context = null

  def this(source: Source) = this(source.getLines.mkString) 
  def this(file: File) = this(Source.fromFile(file).getLines.mkString)

  def render(): String = render(Context())
  def render(context: Context): String = {
    this.context = context
    scripts.clear
    parseStatement
    ScalaInterpreter.evaluate(scripts.mkString(";"), context).toString
  }

  private def parseExpression = {
    var lastPos = 0
    var buf = new ListBuffer[String]
    Template.EXPR_PATTERN.findAllIn(source).matchData.foreach { m =>
      var expr = m.group(1)
      if(lastPos != m.start) buf += quoteStr(source.substring(lastPos, m.start))
      buf += expr2str(expr)
      lastPos = m.end
    }
    if(lastPos != source.length) buf += quoteStr(source.substring(lastPos, source.length))
    scripts += (preamble + " += " + buf.mkString(" + "))
  }
  private def parseStatement = {
    var lastPos = 0
    scripts += "var " + preamble + ": String = \"\"";
    Template.STMT_PATTERN.findAllIn(source).matchData.foreach { m =>
      val expr = m.group(1)
      if(lastPos != m.start)
        scripts += parseExpr(source.substring(lastPos, m.start))
      scripts += expr
      lastPos = m.end
    }
    if(lastPos > 0) {
      if(lastPos != source.length) scripts += preamble + "+=" + quoteStr(trim0(source.substring(lastPos, source.length)))
    } else {
      parseExpression 
    }
    scripts += preamble + ".toString"
  }
  /*
  private def buildTemplateSource = {
    var result = ScalaInterpreter.parse(scripts.mkString)
    result.successful match {
      case true  =>
        var code = "import scala.collection.mutable.{Map, HashMap}\n" +
                   "object " + className + " {\n" +
                   "  def render(ctx: Map[Any, String]) = {\n" +
                   "    var " + preamble + " = \"\"\n" +
                   "    " + scripts.mkString(";\n") +
                   "    buf\n" + 
                   "  }\n" +
                   "}"
        var sourceFileName = className + ".scala"
        var writer = new FileWriter(sourceFileName)
        writer.write(code)
        writer.close()

        //compile
        //(new compiler.Run) compile(List(sourceFileName))
      case false =>
        println(result)
    }
  }
  */
  private def trim0(input: String): String = {
    if(input(0) == '\n') input.substring(1)
    else input
  }
  private def parseExpr(input: String): String = {
    var buf = new ListBuffer[String]
    var lastPos = 0
    var in: String = trim0(input)
    Template.EXPR_PATTERN.findAllIn(in).matchData.foreach { m =>
      var expr = m.group(1)
      if(lastPos != m.start) buf += quoteStr(in.substring(lastPos, m.start))
      buf += expr2str(expr)
      lastPos = m.end
    }
    if(lastPos != in.length) buf += quoteStr(in.substring(lastPos, in.length))
    preamble + " += " + buf.mkString(" + ")
  }
  private def quoteStr(text: String) =  "\"" + text.replaceAll("\"", "\\\\\"")
                                        .replaceAll("\n", "\\\\n").replaceAll("\t", "\\\\t") + "\""
  //private def quoteStr(text: String) =  "\"" + text.replaceAll("\"", "\\\"").replaceAll("\n", "\\\\n") + "\""
  private def expr2str(expr: String) = "(" + expr + ").toString"
}

object DreadLocks 
{
  def main(args: Array[String]) = {
    var source   = Source.fromFile("index.shtml")
    val template = Template(source)
    val context  = Context("restraunts" -> List("Baker Bounce", "Great Burger", "FELLOWS"), 
                           "haveSideMenu" -> true, 
                           "sideMenus" -> HashMap("potato" -> "French fries", "coleslaw" -> "coleslaw"))
    //val context = Context("likeJunkFood" -> true, "iLikeMac" -> "I like MacDonald.", "iDontLikeMac" -> "I don't like MacDonald.")
    println(template.render(context))
  }
}
