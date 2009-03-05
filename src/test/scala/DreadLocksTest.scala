import scala.collection.mutable.HashMap
import org.scalalites.dreadlocks._
import org.scalatest.FunSuite
import java.io.File
import scala.io.Source

class DreadLocksTest extends FunSuite {
  def examplTmplateFile(filename: String) = List("src", "test", "resources", "examples", filename).mkString(File.separator)

/*  test("Render template contains foreach expression.") {*/
    //val tmpl = examplTmplateFile("foreach.shtml")
    //val source = Source.fromFile(tmpl).getLines.mkString
    //val template = Template(source)
    //val output = template.render(Context("books" -> List("The Great Gatzby", "Tender is the Night")))
    //val expect = "F.Scotto.Fitzgerald wrote:\n * The Great Gatzby\n * Tender is the Night\n"
    //assert(output === expect)
  //}
  //test("Render template contains if-else statements.") {
    //val tmpl = examplTmplateFile("if-else.shtml")
    //val source = Source.fromFile(tmpl).getLines.mkString
    //val template = Template(source)
    //var output = template.render(Context("dynamite" -> true))
    //var expect = "B.90.W.60.H.88\n"
    //assert(output === expect)
    //// else case
    //output = template.render(Context("dynamite" -> false))
    //expect = "B.80.W.80.H.80\n"
    //assert(output === expect)
  //}
  //test("Render template contains to reference element in a sequence.") {
    //val tmpl = examplTmplateFile("reference-seq.shtml")
    //val source = Source.fromFile(tmpl).getLines.mkString
    //val template = Template(source)
    //var langs = List("Scala", "C++", "Ruby")
    //val output = template.render(Context("langs" -> langs))
    //assert(output === "Programming languages: " + langs.mkString(", ") + "\n")
  //}
  //test("Render template contains to reference element in a map.") {
    //val tmpl = examplTmplateFile("reference-map.shtml")
    //val source = Source.fromFile(tmpl).getLines.mkString
    //val template = Template(source)
    //var persona = HashMap("first_name" -> "Rakuto", "last_name" -> "Furutani")
    //val output = template.render(Context("persona" -> persona))
    //assert(output === "Hello, my name is Rakuto Furutani\n")
  //}
  
}
