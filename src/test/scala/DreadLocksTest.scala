import scala.collection.mutable.HashMap
import org.scalalites.dreadlocks._
import org.scalatest.FunSuite
import java.io.File
import scala.io.Source

class DreadLocksTest extends FunSuite {
  def examplTmplateFile(filename: String) = List("src", "test", "resources", "examples", filename).mkString(File.separator)

  val templateRootPath = List("src", "test", "resources", "examples").mkString(File.separator)
  Template.setTemplateRootPath(templateRootPath)

  test("variables.txt") {
    val tmpl = examplTmplateFile("variables.txt") 
    val source = Source.fromFile(tmpl)
    val template = Template(source)
    val expect = "Rakuto Furutani\n"
    val context = Context("first_name" -> "Rakuto", "last_name" -> "Furutani")
    val output = template.render(context)
    assert(output === expect)
  }
  test("foreach.shtml") {
    val tmpl = examplTmplateFile("foreach.shtml")
    val source = Source.fromFile(tmpl)
    val template = Template(source)
    val output = template.render(Context("books" -> List("The Great Gatzby", "Tender is the Night")))
    val expect = "<p class=\"label\">F.Scotto.Fitzgerald wrote:</p>\n<ul>\n<li>The Great Gatzby</li>\n<li>Tender is the Night</li>\n</ul>\n<p>and more.</p>\n"
    assert(output === expect)
  }
  //test("foreach-ja.shtml - with multi byte cahracters") {
    //val tmpl = examplTmplateFile("foreach-ja.shtml") 
    //val source = Source.fromFile(tmpl).getLines.mkString
    //val template = Template(source)
    //val output = template.render(Context("book" -> List("The Great Gatzby", "Tender is the Night")))
    //val expect = "フィッツジェラルド\n"
    //assert(output === expect)
  //}
  test("foreach2.shtml - with a parameter contains tuples") {
    val tmpl     = examplTmplateFile("foreach2.shtml")
    val source   = Source.fromFile(tmpl)
    val template = Template(source)
    val context  = Context("cafes" -> List(("Authentic", "Akasaka, Tokyo"), ("FELLOWS", "Komazawa, Tokyo")))
    val output   = template.render(context)
    val expect   = "Hamburger Cafe:\n * Authentic in Akasaka, Tokyo\n * FELLOWS in Komazawa, Tokyo\n"
    assert(output === expect)
  }
  test("unary-op.txt") {
    val tmpl = examplTmplateFile("unary-op.txt") 
    val source = Source.fromFile(tmpl)
    val template = Template(source)
    val context = Context("onDiet" -> false)
    val output = template.render(context)
    val expect = "Let's get to a bite to eat!\n"
    assert(output === expect)
  }
  test("nest-if-foreach.txt") {
    val tmpl = examplTmplateFile("nest-if-foreach.txt") 
    val source = Source.fromFile(tmpl)
    val template = Template(source)
    val context = Context("diet" -> false, "hamburgers" -> List("Baker Bounce", "FELLOWS"))
    val output = template.render(context)
    val expect = "Baker Bounce\nFELLOWS\n"
    assert(output === expect)
  }
  test("Render template contains if-else statements.") {
    val tmpl = examplTmplateFile("if-else.shtml")
    val source = Source.fromFile(tmpl).getLines.mkString
    val template = Template(source)
    var output = template.render(Context("dynamite" -> true))
    var expect = "B.90.W.60.H.88\n"
    assert(output === expect)
    // else case
    output = template.render(Context("dynamite" -> false))
    expect = "B.80.W.80.H.80\n"
    assert(output === expect)
  }
  test("Render template contains to reference element in a sequence.") {
    val tmpl = examplTmplateFile("reference-seq.shtml")
    val source = Source.fromFile(tmpl).getLines.mkString
    val template = Template(source)
    var langs = List("Scala", "C++", "Ruby")
    val output = template.render(Context("langs" -> langs))
    assert(output === "Programming languages: " + langs.mkString(", ") + "\n")
  }
  test("Render template contains to reference element in a map.") {
    val tmpl = examplTmplateFile("reference-map.shtml")
    val source = Source.fromFile(tmpl).getLines.mkString
    val template = Template(source)
    var persona = HashMap("first_name" -> "Rakuto", "last_name" -> "Furutani")
    val output = template.render(Context("persona" -> persona))
    assert(output === "Hello, my name is Rakuto Furutani\n")
  }
  test("Import other template file in template file.") {
    val tmpl = examplTmplateFile("yields.shtml") 
    val source = Source.fromFile(tmpl)
    val template = Template(source)
    var output = template.render(Context())
    assert(output === "Hello from parent.\nHey boy from partial.shtml.\n")
  }
}
