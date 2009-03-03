import sbt._
import java.io.File

class DreadLocksProject(info: ProjectInfo) extends DefaultProject(info)
{
    val scalatest = "org.scalatest" % "scalatest" % "0.9.4"
    val compiler  = "org.scala-lang" % "scala-compiler" % "2.7.3"
    //override def mainClass = Some("org.scalalites.dreadlocks.ScalaLanguageParser")
    override def mainClass = Some("org.scalalites.dreadlocks.ScalaInterpreter")
    //override def mainClass = Some("Reflection")
}
